#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
extern "C" {
  #include "mplib.h"
}
#include <Rcpp.h>
using namespace Rcpp;

// These must match the order of components within a "knot" object
// in 'metaplot'
const int index_x = 0;
const int index_y = 1;
const int index_dir_left = 2;
const int index_dir_right = 3;
const int index_cp_left_x = 4;
const int index_cp_right_x = 5;
const int index_cp_left_y = 6;
const int index_cp_right_y = 7;
const int index_curl_left = 8;
const int index_curl_right = 9;
const int index_tension_left = 10;
const int index_tension_right = 11;

const int num_knot_components = 12;

double knot_component(int i, 
                      int component, 
                      const NumericVector &knotArray, 
                      int nKnots) {
    return knotArray[nKnots*component + i];
}

double knot_x(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_x, knotArray, nKnots);
}

double knot_y(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_y, knotArray, nKnots);
}

double knot_dir_left(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_dir_left, knotArray, nKnots);
}

double knot_dir_right(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_dir_right, knotArray, nKnots);
}

double knot_cp_left_x(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_cp_left_x, knotArray, nKnots);
}

double knot_cp_right_x(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_cp_right_x, knotArray, nKnots);
}

double knot_cp_left_y(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_cp_left_y, knotArray, nKnots);
}

double knot_cp_right_y(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_cp_right_y, knotArray, nKnots);
}

double knot_curl_left(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_curl_left, knotArray, nKnots);
}

double knot_curl_right(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_curl_right, knotArray, nKnots);
}

double knot_tension_left(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_tension_left, knotArray, nKnots);
}

double knot_tension_right(int i, const NumericVector &knotArray, int nKnots) {
    return knot_component(i, index_tension_right, knotArray, nKnots);
}

mp_knot append_knot(MP mp, mp_knot prev, int i, 
                    const NumericVector &knotArray, int nKnots,
                    bool first, bool last, bool cycle) {
    int mp_result;
    // x/y locations for knots are not allowed to be missing values
    mp_knot knot = mp_append_knot(mp, prev,
                                  knot_x(i, knotArray, nKnots),
                                  knot_y(i, knotArray, nKnots));
    if (!knot) exit(EXIT_FAILURE);
    double dir_left = knot_dir_left(i, knotArray, nKnots);
    double dir_right = knot_dir_right(i, knotArray, nKnots);
    if (!isnan(dir_left) || !isnan(dir_right)) {
        // FIXME
        // We cannot handle different left/right directions in MPlib, 
        // so need to send a warning!
        // (Left currently wins if both given and not the same)
        double dir_x, dir_y;
        if (!isnan(dir_left)) {
            dir_x = cos(dir_left/180*M_PI);
            dir_y = sin(dir_left/180*M_PI);
        } else {
            dir_x = cos(dir_right/180*M_PI);
            dir_y = sin(dir_right/180*M_PI);
        }
        mp_result = mp_set_knot_direction(mp, knot, dir_x, dir_y);
        if (!mp_result) exit(EXIT_FAILURE);
    }
    double curl_left = knot_curl_left(i, knotArray, nKnots);
    double curl_right = knot_curl_right(i, knotArray, nKnots);
    if (!isnan(curl_left)) {
        mp_result = mp_set_knot_left_curl(mp, knot, curl_left);
        if (!mp_result) exit(EXIT_FAILURE);
    }
    if (!isnan(curl_right)) {
        mp_result = mp_set_knot_right_curl(mp, knot, curl_left);
        if (!mp_result) exit(EXIT_FAILURE);
    }
    // For a non-cyclic path, in the absence of direction (and explicit curl),
    // the first knit right curl should default to 1
    if (first && !cycle && 
        isnan(dir_left) && isnan(dir_right) && 
        isnan(curl_right)) {
        mp_result = mp_set_knot_right_curl(mp, knot, 1.0);
        if (!mp_result) exit(EXIT_FAILURE);
    }
    // For a non-cyclic path, in the absence of direction (and explicit curl),
    // the last knot left curl should default to 1
    if (last && !cycle && 
        isnan(dir_left) && isnan(dir_right) && 
        isnan(curl_left)) {
        mp_result = mp_set_knot_left_curl(mp, knot, 1.0);
        if (!mp_result) exit(EXIT_FAILURE);
    }
    double tension_left = knot_tension_left(i, knotArray, nKnots);
    double tension_right = knot_tension_right(i, knotArray, nKnots);
    if (!isnan(tension_left)) {
        mp_result = mp_set_knot_left_tension(mp, knot, tension_left);
        if (!mp_result) exit(EXIT_FAILURE);
    }
    if (!isnan(tension_right)) {
        mp_result = mp_set_knot_right_tension(mp, knot, tension_left);
        if (!mp_result) exit(EXIT_FAILURE);
    }
    // BOTH x and y have to be non-NaN to set the explicit control point
    double cp_left_x = knot_cp_left_x(i, knotArray, nKnots);
    double cp_left_y = knot_cp_left_y(i, knotArray, nKnots);
    if (!isnan(cp_left_x) && !isnan(cp_left_x)) {
        mp_result = mp_set_knot_left_control(mp, knot, cp_left_x, cp_left_y);
        if (!mp_result) exit(EXIT_FAILURE);
    }
    double cp_right_x = knot_cp_right_x(i, knotArray, nKnots);
    double cp_right_y = knot_cp_right_y(i, knotArray, nKnots);
    if (!isnan(cp_right_x) && !isnan(cp_right_x)) {
        mp_result = mp_set_knot_right_control(mp, knot, cp_right_x, cp_right_y);
        if (!mp_result) exit(EXIT_FAILURE);
    }
    return knot;
}

// 'x' is a 2D numeric array (a single path)
//     (to allow multiple paths [of same dimension] at once)
//     1:nPaths x 1:numComponentsInAKnit x 1:nKnots
// [[Rcpp::export]]
NumericVector solvePath(const NumericVector &x, int nKnots, bool cycle) {
    MP mp;
    mp_knot first, prev, current;
    MP_options* opt = mp_options();
    int i, mp_result, n;
    // Result is 2D numeric array
    //     1:nKnots x 1:4(control points) (to allow for cycle)
    NumericVector result(nKnots*8);
    opt -> command_line = NULL;
    opt -> noninteractive = 1;
    mp = mp_initialize(opt);
    if (!mp) exit(EXIT_FAILURE);
    first = append_knot(mp, NULL, 0, x, nKnots, true, false, cycle);
    prev = first;
    // There will be at least two knots (because this is a path)
    for (i=1; i < nKnots; i++) {
        current = append_knot(mp, prev, i, x, nKnots, 
                              false, i == nKnots - 1, cycle);
        prev = current;
    }
    if (cycle) {
        mp_result = mp_close_path_cycle(mp, current, first);
        n = nKnots;
    } else {
        mp_result = mp_close_path(mp, current, first);
        n = nKnots - 1;
    }
    if (!mp_result) exit(EXIT_FAILURE);
    // printf("cycle = %d\n", cycle);
    mp_result = mp_solve_path(mp, first);
    if (!mp_result) exit(EXIT_FAILURE);
    current = first;
    for (i=0; i < n; i++) {
        result[i*8 + 0] = mp_number_as_double(mp, current->x_coord);
        result[i*8 + 1] = mp_number_as_double(mp, current->y_coord);
        result[i*8 + 2] = mp_number_as_double(mp, current->right_x);
        result[i*8 + 3] = mp_number_as_double(mp, current->right_y);
        result[i*8 + 4] = mp_number_as_double(mp, current->next->left_x);
        result[i*8 + 5] = mp_number_as_double(mp, current->next->left_y);
        result[i*8 + 6] = mp_number_as_double(mp, current->next->x_coord);
        result[i*8 + 7] = mp_number_as_double(mp, current->next->y_coord);
        current = current->next;
    }
    mp_free_path(mp, first);
    mp_finish(mp);
    free(opt);
    return result;
}
