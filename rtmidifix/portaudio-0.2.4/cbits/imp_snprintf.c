/* Define __imp__snprintf for MSVC-compiled object files.
 *
 * MSVC-compiled .obj files reference __declspec(dllimport) _snprintf,
 * which becomes the COFF symbol __imp__snprintf. We provide it here
 * by pointing it to the standard C library's snprintf.
 */

int snprintf(char *, unsigned long long, const char *, ...);

/* __imp__snprintf must be a DATA symbol (not TEXT) so that COFF
 * __declspec(dllimport) resolution works correctly. The linker will
 * redirect references from __imp__snprintf to this pointer value,
 * which is the address of snprintf itself. */
__attribute__((used))
int (*__imp__snprintf)(char *, unsigned long long, const char *, ...) = snprintf;
