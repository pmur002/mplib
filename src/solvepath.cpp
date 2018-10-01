#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern "C" {
  #include "mplib.h"
}
#include <Rcpp.h>

// These must match the order of components within a "knot" object
// in 'metaplot'
const int path_x = 0;
const int path_y = 1;

const int num_knot_components = 12;

// 'x' is a 3D numeric array
//     1:nKnots x 1:numComponentsInAKnit x 1:nPaths
// [[Rcpp::export]]
Rcpp::NumericVector solvePath(const Rcpp::NumericVector &x, 
                              int nKnots, int nPaths) {
    // Result is 3D numeric array
    //     1:nKnots x 1:4(control points) x 1:nPaths
    Rcpp::NumericVector result(nKnots*4*nPaths);
    MP mp;
    mp_knot first, p, q;
    MP_options* opt = mp_options();
    opt -> command_line = NULL;
    opt -> noninteractive = 1;
    mp = mp_initialize(opt);
    if (!mp) exit (EXIT_FAILURE);
    p = mp_append_knot(mp, NULL, 
                       x[path_x], 
                       x[path_y]);
    if (!p) exit (EXIT_FAILURE) ;
    q = mp_append_knot(mp, p, 
                       x[num_knot_components + path_x], 
                       x[num_knot_components + path_y]);
    mp_close_path_cycle(mp, q, p);
    mp_solve_path(mp, p);
    result[0] = mp_number_as_double(mp, p->x_coord);
    result[1] = mp_number_as_double(mp, p->y_coord);
    result[2] = mp_number_as_double(mp, p->right_x);
    result[3] = mp_number_as_double(mp, p->right_y);
    result[4] = mp_number_as_double(mp, q->left_x);
    result[5] = mp_number_as_double(mp, q->left_y);
    result[6] = mp_number_as_double(mp, q->x_coord);
    result[7] = mp_number_as_double(mp, q->y_coord);
    mp_free_path(mp, p);
    mp_finish(mp);
    free(opt);
    return result;
}
