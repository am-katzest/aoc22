$fn = 128;
difference () {
  sphere (r=10);
  difference () {
    union () {
      rotate ([0.0,0.0,($t*3.141592653589793*57.29577951308232)]) {
        union () {
          translate ([0.0, 12.0, 0]) {
            cylinder (h=100, r=5, center=true);
          }
          translate ([12.0, 7.347880794884119E-16, 0]) {
            cylinder (h=100, r=5, center=true);
          }
          translate ([1.4695761589768238E-15, -12.0, 0]) {
            cylinder (h=100, r=5, center=true);
          }
          translate ([-12.0, -2.204364238465236E-15, 0]) {
            cylinder (h=100, r=5, center=true);
          }
        }
      }
      mirror ([1, 0, 1]) {
        rotate ([0.0,0.0,($t*3.141592653589793*57.29577951308232)]) {
          union () {
            translate ([0.0, 12.0, 0]) {
              cylinder (h=100, r=5, center=true);
            }
            translate ([12.0, 7.347880794884119E-16, 0]) {
              cylinder (h=100, r=5, center=true);
            }
            translate ([1.4695761589768238E-15, -12.0, 0]) {
              cylinder (h=100, r=5, center=true);
            }
            translate ([-12.0, -2.204364238465236E-15, 0]) {
              cylinder (h=100, r=5, center=true);
            }
          }
        }
      }
      mirror ([0, 1, 1]) {
        rotate ([0.0,0.0,($t*3.141592653589793*57.29577951308232)]) {
          union () {
            translate ([0.0, 12.0, 0]) {
              cylinder (h=100, r=5, center=true);
            }
            translate ([12.0, 7.347880794884119E-16, 0]) {
              cylinder (h=100, r=5, center=true);
            }
            translate ([1.4695761589768238E-15, -12.0, 0]) {
              cylinder (h=100, r=5, center=true);
            }
            translate ([-12.0, -2.204364238465236E-15, 0]) {
              cylinder (h=100, r=5, center=true);
            }
          }
        }
      }
    }
    union () {
      rotate ([0.0,0.0,($t*3.141592653589793*57.29577951308232)]) {
        union () {
          translate ([0.0, 10.0, 0]) {
            cylinder (h=1001, r=4, center=true);
          }
          translate ([10.0, 6.123233995736766E-16, 0]) {
            cylinder (h=1001, r=4, center=true);
          }
          translate ([1.2246467991473533E-15, -10.0, 0]) {
            cylinder (h=1001, r=4, center=true);
          }
          translate ([-10.0, -1.8369701987210296E-15, 0]) {
            cylinder (h=1001, r=4, center=true);
          }
        }
      }
      mirror ([1, 0, 1]) {
        rotate ([0.0,0.0,($t*3.141592653589793*57.29577951308232)]) {
          union () {
            translate ([0.0, 10.0, 0]) {
              cylinder (h=1001, r=4, center=true);
            }
            translate ([10.0, 6.123233995736766E-16, 0]) {
              cylinder (h=1001, r=4, center=true);
            }
            translate ([1.2246467991473533E-15, -10.0, 0]) {
              cylinder (h=1001, r=4, center=true);
            }
            translate ([-10.0, -1.8369701987210296E-15, 0]) {
              cylinder (h=1001, r=4, center=true);
            }
          }
        }
      }
      mirror ([0, 1, 1]) {
        rotate ([0.0,0.0,($t*3.141592653589793*57.29577951308232)]) {
          union () {
            translate ([0.0, 10.0, 0]) {
              cylinder (h=1001, r=4, center=true);
            }
            translate ([10.0, 6.123233995736766E-16, 0]) {
              cylinder (h=1001, r=4, center=true);
            }
            translate ([1.2246467991473533E-15, -10.0, 0]) {
              cylinder (h=1001, r=4, center=true);
            }
            translate ([-10.0, -1.8369701987210296E-15, 0]) {
              cylinder (h=1001, r=4, center=true);
            }
          }
        }
      }
    }
  }
}
