deltaPower from to curve deltaRatio offsetY = let
    power = 10.0 ** (2.0 * realToFrac curve - 1.0) -- center 0.5 expmap
    range = to - from
    offsetRatio = offsetY ** (1 / power) -- offsetY \in [0, 1], not a bug yet
    in range * ((realToFrac deltaRatio + offsetRatio) ** power - offsetRatio ** power)