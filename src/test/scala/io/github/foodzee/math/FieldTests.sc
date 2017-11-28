import io.github.foodzee.math.galois.GaloisField

val gf = new GaloisField(2, 4, 0x13 /*0b10011*/)
for (ei <- gf.m.zipWithIndex) println(ei)