$fn = 256;
difference () {
  sphere (r=18);
  intersection () {
    sphere (r=20);
    union () {
      rotate ([0.0,0.0,38158.98915571283]) {
        union () {
          translate ([0.0, 16.0, 0]) {
            cylinder (h=100, r=8, center=true);
          }
          translate ([16.0, 9.797174393178826E-16, 0]) {
            cylinder (h=100, r=8, center=true);
          }
          translate ([1.959434878635765E-15, -16.0, 0]) {
            cylinder (h=100, r=8, center=true);
          }
          translate ([-16.0, -2.9391523179536475E-15, 0]) {
            cylinder (h=100, r=8, center=true);
          }
        }
      }
      mirror ([1, 0, 1]) {
        rotate ([0.0,0.0,38158.98915571283]) {
          union () {
            translate ([0.0, 16.0, 0]) {
              cylinder (h=100, r=8, center=true);
            }
            translate ([16.0, 9.797174393178826E-16, 0]) {
              cylinder (h=100, r=8, center=true);
            }
            translate ([1.959434878635765E-15, -16.0, 0]) {
              cylinder (h=100, r=8, center=true);
            }
            translate ([-16.0, -2.9391523179536475E-15, 0]) {
              cylinder (h=100, r=8, center=true);
            }
          }
        }
      }
      mirror ([0, 1, 1]) {
        rotate ([0.0,0.0,38158.98915571283]) {
          union () {
            translate ([0.0, 16.0, 0]) {
              cylinder (h=100, r=8, center=true);
            }
            translate ([16.0, 9.797174393178826E-16, 0]) {
              cylinder (h=100, r=8, center=true);
            }
            translate ([1.959434878635765E-15, -16.0, 0]) {
              cylinder (h=100, r=8, center=true);
            }
            translate ([-16.0, -2.9391523179536475E-15, 0]) {
              cylinder (h=100, r=8, center=true);
            }
          }
        }
      }
    }
  }
}
