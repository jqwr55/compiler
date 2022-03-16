#include <common.h>


template<typename T, u32 n> 
struct vec {
    T arr[n];
    T& operator [](u32 index) {
        return arr[index];
    }
};
template<typename T, u32 m, u32 n>
struct Mat {
    //vec<T, m> bases[n];
    T arr[m][n];
    T& operator [](u32 i0) {
        return bases[i0];
    }
};
template<typename T, u32 n>
void PrintVec(vec<T, n> vec) {
    for(u32 k = 0; k < n; k++) {
        global_print("f, c", vec.arr[k], '\t');
    }
    global_print("c", '\n');
}
template<u32 m, u32 n>
void PrintMat(Mat<f32, m,n> mat) {
    for(u32 i = 0; i < m; i++) {
        for(u32 k = 0; k < n; k++) {
            global_print("f, c", mat.bases[k].arr[i], '\t');
        }
        global_print("c", '\n');
    }
}

void SolveGauss() {


    //constexpr u32 SIZE = 10;
    printf("Enter number of unknowns: ");
    int n;
    scanf("%d", &n);
    
    f32 a[n][n+1], x[n];

    for(u32 i = 0;i <= n; i++)
    {
        for(u32 j = 0; j <= n+1; j++)
        {
            printf("a[%d][%d] = ",i,j);
            scanf("%f", &a[i][j]);
        }
    }
    
/* Applying Gauss Elimination */
    for(u32 i = 0; i <= n-1; i++) {
        if(a[i][i] == 0.0) {
            printf("Mathematical Error!");
            exit(0);
        }
        for(u32 j = i+1; j <= n; j++) {
            f32 ratio = a[j][i] / a[i][i];
            
            for(u32 k = 0; k <= n+1; k++) {
                a[j][k] = a[j][k] - ratio * a[i][k];
            }
        }
    }
    /* Obtaining Solution by Back Subsitution */
    x[n] = a[n][n+1] / a[n][n];

    for(u32 i = n-1; i >= 1; i--) {
        x[i] = a[i][n+1];
        for(u32 j = i+1; j <= n; j++) {
            x[i] = x[i] - a[i][j] * x[j];
        }
        x[i] = x[i] / a[i][i];
    }
    /* Displaying Solution */ 
    printf("\nSolution:\n");
    for(u32 i = 0; i <= n; i++) {
        printf("x[%d] = %0.3f\n",i, x[i]);
    }
}

template<typename T, u32 n>
vec<T, n> Gauss(Mat<T, n,n+1> a) {

    for(u32 i = 1; i <= n-1; i++) {
        if(a[i][i] == 0.0) {
            printf("Mathematical Error!");
            exit(0);
        }
        for(u32 j = i+1; j <= n; j++) {
            f32 ratio = a[j][i] / a[i][i];
            
            for(u32 k = 1; k <= n+1; k++) {
                a[j][k] = a[j][k] - ratio * a[i][k];
            }
        }
    }

    /* Obtaining Solution by Back Subsitution */
    vec<T, n> x;
    x[n] = a[n][n+1] / a[n][n];

    for(u32 i = n-1; i >= 1; i--) {
        x[i] = a[i][n+1];
        for(u32 j = i+1; j <= n; j++) {
            x[i] = x[i] - a[i][j] * x[j];
        }
        x[i] = x[i] / a[i][i];
    }
    return x;
}

i32 main() {

    /*
    0, 0, 0, 0,
    1, 1, 1, 1,
    1, 1, 1, 1,
    1, 1, 1, 1
    */

    init_global_state(0, 0, 512);
    SolveGauss();
    //PrintMat(mat);
    //global_print("c", '\n');
    //auto x = gauss(mat);
    ////PrintVec(x);
    //global_io_flush();

    return 0;
}