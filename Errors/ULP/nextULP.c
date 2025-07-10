#include <stdio.h>
#include <math.h>

int main() {
    double x = pow(80,10);
    double next = nextafter(x, INFINITY);

    printf("x      = %.17g\n", x);
    printf("nextULP = %.17g\n", next);
    printf("ULP(x) = %.17g\n", next - x);

    return 0;
}