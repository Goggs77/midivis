// cbits/Eq12.cpp
// 12-band parametric IIR EQ + mandatory Nyquist guard, stereo interleaved,
// processed in place on a CFloat buffer.
//
// Contract with Haskell (Midivis.Synth.Eq):
//   void* eq12_new()                                    -- opaque state
//   void  eq12_free(void*)
//   void  eq12_process(void* st, const float* params,
//                      float* buf, int nFrames, double sr)
//   params: 48 floats per block, band-major: for b in 0..11
//     [4b+0] freq  (Hz, clamped to [20, 0.98*Nyquist])
//     [4b+1] gain  (dB, clamped to [-24, 24])
//     [4b+2] shape (0..1, piecewise -> 7 filter types)
//     [4b+3] Q     (clamped to [0.1, 10])
//
// Per block the state one-pole-lerps the params toward these targets
// (k = 0.3), then recomputes the band coefficients from the smoothed
// values, so modulation glides instead of popping.  The guard band is a
// fixed 8th-order Butterworth lowpass at 0.98 x Nyquist, always active
// ("unconditional"); its 4 cascaded biquads sit after the 12 variable
// bands (16 stages per channel total).
//
// Per-band smart bypass: gain-type shapes (low shelf / peak / high shelf)
// with |gain| < 1e-6 are skipped in the per-sample cascade; frequency-type
// shapes (notch / bandpass / lowpass / highpass) always run.  The guard
// never bypasses.

#include <cmath>
#include <cstring>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

extern "C" {

static constexpr int kCh   = 2;      // stereo
static constexpr int kBand = 12;     // variable bands
static constexpr int kGst  = 4;      // guard: 8th-order Butterworth = 4 biquads
static constexpr int kSt   = kBand + kGst;   // 16 stages per channel
static constexpr double kSm  = 0.3;  // one-pole lerp factor per block
static constexpr double kGB  = 0.98; // guard cutoff as a fraction of Nyquist

static inline double clampd(double v, double lo, double hi) {
    return v < lo ? lo : (v > hi ? hi : v);
}

struct Eq12State {
    double sm[48];                         // smoothed params
    double x1[kCh][kSt], x2[kCh][kSt];     // biquad delay state
    double y1[kCh][kSt], y2[kCh][kSt];
};

void* eq12_new() {
    auto* st = new Eq12State();
    std::memset(st, 0, sizeof(Eq12State));
    for (int b = 0; b < kBand; ++b) {
        st->sm[4*b + 0] = 1000.0;   // freq
        st->sm[4*b + 1] = 0.0;      // gain
        st->sm[4*b + 2] = 1.0/7.0;  // shape: peak
        st->sm[4*b + 3] = 1.0;      // Q
    }
    return st;
}

void eq12_free(void* p) { delete static_cast<Eq12State*>(p); }

// ---------------------------------------------------------------------------
// RBJ cookbook biquads (transposed direct-form II applied per sample)
// ---------------------------------------------------------------------------

struct Coeffs { double b0, b1, b2, a1, a2; };

enum Shape { kLowShelf, kPeak, kHighShelf, kNotch, kBandpass, kLowpass, kHighpass };

static inline Shape shapeOf(double s) {
    int i = static_cast<int>(7.0 * clampd(s, 0.0, 1.0));
    if (i > 6) i = 6;
    return static_cast<Shape>(i);
}

static Coeffs lowShelfC(double g, double f0, double q, double sr) {
    (void)q;   // shelf slope S = 1 fixed
    double a  = std::pow(10.0, g / 40.0);
    double w0 = 2.0 * M_PI * f0 / sr;
    double c  = std::cos(w0), s = std::sin(w0);
    double b  = std::sqrt(2.0 * a);          // shelf slope S = 1
    double bb = 2.0 * std::sqrt(a) * b;
    double a0 = (a + 1) + (a - 1) * c + bb * s;
    Coeffs r;
    r.b0 = a * ((a + 1) - (a - 1) * c + bb * s) / a0;
    r.b1 = 2.0 * a * ((a - 1) - (a + 1) * c) / a0;
    r.b2 = a * ((a + 1) - (a - 1) * c - bb * s) / a0;
    r.a1 = -2.0 * ((a - 1) + (a + 1) * c) / a0;
    r.a2 = ((a + 1) + (a - 1) * c - bb * s) / a0;
    return r;
}

static Coeffs highShelfC(double g, double f0, double q, double sr) {
    (void)q;
    double a  = std::pow(10.0, g / 40.0);
    double w0 = 2.0 * M_PI * f0 / sr;
    double c  = std::cos(w0), s = std::sin(w0);
    double b  = 1.0 / std::sqrt(2.0 * a);
    double bb = 2.0 * std::sqrt(a) * b;
    double a0 = (a + 1) + (a - 1) * c + bb * s;
    Coeffs r;
    r.b0 = a * ((a + 1) - (a - 1) * c + bb * s) / a0;
    r.b1 = -2.0 * a * ((a - 1) + (a + 1) * c) / a0;
    r.b2 = a * ((a + 1) - (a - 1) * c - bb * s) / a0;
    r.a1 = -2.0 * ((a - 1) + (a + 1) * c) / a0;
    r.a2 = ((a + 1) + (a - 1) * c - bb * s) / a0;
    return r;
}

static Coeffs peakC(double g, double f0, double q, double sr) {
    double a  = std::pow(10.0, g / 40.0);
    double w0 = 2.0 * M_PI * f0 / sr;
    double c  = std::cos(w0), s = std::sin(w0);
    double al = s / (2.0 * q);
    double a0 = 1.0 + al / a;
    Coeffs r;
    r.b0 = (1.0 + al * a) / a0;
    r.b1 = -2.0 * c / a0;
    r.b2 = (1.0 - al * a) / a0;
    r.a1 = -2.0 * c / a0;
    r.a2 = (1.0 - al / a) / a0;
    return r;
}

static Coeffs notchC(double f0, double q, double sr) {
    double w0 = 2.0 * M_PI * f0 / sr;
    double c  = std::cos(w0), s = std::sin(w0);
    double al = s / (2.0 * q);
    double a0 = 1.0 + al;
    Coeffs r;
    r.b0 = 1.0 / a0;
    r.b1 = -2.0 * c / a0;
    r.b2 = 1.0 / a0;
    r.a1 = r.b1;
    r.a2 = (1.0 - al) / a0;
    return r;
}

static Coeffs bandpassC(double f0, double q, double sr) {
    double w0 = 2.0 * M_PI * f0 / sr;
    double c  = std::cos(w0), s = std::sin(w0);
    double al = s / (2.0 * q);
    double a0 = 1.0 + al;
    Coeffs r;
    r.b0 = al / a0;
    r.b1 = 0.0;
    r.b2 = -al / a0;
    r.a1 = -2.0 * c / a0;
    r.a2 = (1.0 - al) / a0;
    return r;
}

static Coeffs lowpassC(double f0, double q, double sr) {
    double w0 = 2.0 * M_PI * f0 / sr;
    double c  = std::cos(w0), s = std::sin(w0);
    double al = s / (2.0 * q);
    double a0 = 1.0 + al;
    Coeffs r;
    r.b0 = (1.0 - c) / (2.0 * a0);
    r.b1 = (1.0 - c) / a0;
    r.b2 = r.b0;
    r.a1 = -2.0 * c / a0;
    r.a2 = (1.0 - al) / a0;
    return r;
}

static Coeffs highpassC(double f0, double q, double sr) {
    double w0 = 2.0 * M_PI * f0 / sr;
    double c  = std::cos(w0), s = std::sin(w0);
    double al = s / (2.0 * q);
    double a0 = 1.0 + al;
    Coeffs r;
    r.b0 = (1.0 + c) / (2.0 * a0);
    r.b1 = -(1.0 + c) / a0;
    r.b2 = r.b0;
    r.a1 = -2.0 * c / a0;
    r.a2 = (1.0 - al) / a0;
    return r;
}

// 8th-order Butterworth lowpass = 4 cascaded biquads with these Qs
static constexpr double kButterQ[4] = { 0.5098, 0.6013, 0.8999, 2.5629 };

static inline double biquad(Eq12State* st, int ch, int stage,
                            const Coeffs& c, double x) {
    double x1 = st->x1[ch][stage], x2 = st->x2[ch][stage];
    double y1 = st->y1[ch][stage], y2 = st->y2[ch][stage];
    double y  = c.b0 * x + c.b1 * x1 + c.b2 * x2 - c.a1 * y1 - c.a2 * y2;
    st->x2[ch][stage] = x1;
    st->x1[ch][stage] = x;
    st->y2[ch][stage] = y1;
    st->y1[ch][stage] = y;
    return y;
}

void eq12_process(void* p, const float* params, float* buf,
                  int nFrames, double sr) {
    Eq12State* st = static_cast<Eq12State*>(p);
    double ny = 0.5 * sr;

    // 1. smooth params toward targets (clamped to safe ranges)
    for (int b = 0; b < kBand; ++b) {
        double f = params[4*b + 0], g = params[4*b + 1];
        double s = params[4*b + 2], q = params[4*b + 3];
        st->sm[4*b + 0] += kSm * (clampd(f, 20.0, kGB * ny) - st->sm[4*b + 0]);
        st->sm[4*b + 1] += kSm * (clampd(g, -24.0, 24.0)  - st->sm[4*b + 1]);
        st->sm[4*b + 2] += kSm * (clampd(s,  0.0, 1.0)    - st->sm[4*b + 2]);
        st->sm[4*b + 3] += kSm * (clampd(q,  0.1, 10.0)   - st->sm[4*b + 3]);
    }

    // 2. band coefficients + smart-bypass flags (block-constant)
    Coeffs band[kBand];
    bool   active[kBand];
    for (int b = 0; b < kBand; ++b) {
        double f = st->sm[4*b + 0], g = st->sm[4*b + 1];
        double s = st->sm[4*b + 2], q = st->sm[4*b + 3];
        switch (shapeOf(s)) {
            case kLowShelf:  band[b] = lowShelfC(g, f, q, sr); break;
            case kPeak:      band[b] = peakC(g, f, q, sr);     break;
            case kHighShelf: band[b] = highShelfC(g, f, q, sr); break;
            case kNotch:     band[b] = notchC(f, q, sr);       break;
            case kBandpass:  band[b] = bandpassC(f, q, sr);    break;
            case kLowpass:   band[b] = lowpassC(f, q, sr);     break;
            case kHighpass:  band[b] = highpassC(f, q, sr);    break;
        }
        bool gainType = (s < 3.0 / 7.0);   // low shelf / peak / high shelf
        active[b] = !(gainType && std::fabs(g) < 1e-6);
    }

    // 3. guard: 8th-order Butterworth at kGB x Nyquist (always active)
    Coeffs guardC[kGst];
    for (int i = 0; i < kGst; ++i)
        guardC[i] = lowpassC(kGB * ny, kButterQ[i], sr);

    // 4. per-sample cascade, both channels
    for (int i = 0; i < nFrames; ++i) {
        for (int ch = 0; ch < kCh; ++ch) {
            double x = buf[2 * i + ch];
            for (int b = 0; b < kBand; ++b) {
                if (active[b]) x = biquad(st, ch, b, band[b], x);
            }
            for (int g = 0; g < kGst; ++g)
                x = biquad(st, ch, kBand + g, guardC[g], x);
            buf[2 * i + ch] = static_cast<float>(x);
        }
    }
}

} // extern "C"
