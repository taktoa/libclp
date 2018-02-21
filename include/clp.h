// Copyright (C) 2002, 2003 International Business Machines Corporation
// and others. All Rights Reserved.
//
// This code is licensed under the terms of the Eclipse Public License (EPL).

#ifndef ClpSimplexC_H
#define ClpSimplexC_H

// include all defines and ugly stuff

#include <Coin_C_defines.h>

#if defined(CLP_EXTERN_C)
typedef struct {
    ClpSolve options;
} Clp_Solve;
#else
typedef void Clp_Solve;
#endif

#ifdef __cplusplus
extern "C" {
#endif

    // -------------------------------------------------------------------------

    // A Clp library has a version number of the form
    // `<major>.<minor>.<release>`, where each of `major`, `minor`,
    // and `release` are nonnegative integers.
    //
    // For a checkout of the Clp stable branch, `release` is `9999`.
    //
    // For a checkout of the Clp development branch,
    // `major`, `minor`, and `release` are all `9999`.

    // Clp library version number as string.
    const char* Clp_Version(void);

    // Major number of Clp library version.
    int Clp_VersionMajor(void);

    // Minor number of Clp library version.
    int Clp_VersionMinor(void);

    // Release number of Clp library version.
    int Clp_VersionRelease(void);

    // -------------------------------------------------------------------------

    // These do not have an exact analogue in C++.
    // The user does not need to know structure of `Clp_Simplex` or `Clp_Solve`.
    //
    // For (almost) all `Clp_*` functions outside this group there is an
    // exact C++ analogue created by taking the first parameter out, removing
    // the `Clp_` from name and applying the method to an object of
    // type `ClpSimplex`.
    //
    // Similarly, for all `ClpSolve_*` functions there is an exact C++
    // analogue created by taking the first parameter out, removing the
    // `ClpSolve_` from name and applying the method to an object of
    // type `ClpSolve`.

    // Default constructor
    Clp_Simplex* Clp_newModel(void);

    // Destructor
    void Clp_deleteModel(
        Clp_Simplex* model);

    // Default constructor
    Clp_Solve* ClpSolve_new(void);

    // Destructor
    void ClpSolve_delete(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // Loads a problem (the constraints on the rows are given by lower and
    // upper bounds). If a pointer is NULL then the following values are
    // the default:
    // <ul>
    // <li><code>colub</code>: all columns have upper bound infinity     </li>
    // <li><code>collb</code>: all columns have lower bound 0            </li>
    // <li><code>rowub</code>: all rows have upper bound infinity        </li>
    // <li><code>rowlb</code>: all rows have lower bound -infinity       </li>
    // <li><code>obj</code>:   all variables have 0 objective coefficient</li>
    // </ul>
    //
    // Just like the other loadProblem() method except that the matrix is
    // given in a standard column major ordered format (without gaps).
    void Clp_loadProblem(
        Clp_Simplex*        model,
        int                 numcols,
        int                 numrows,
        const CoinBigIndex* start,
        const int*          index,
        const double*       value,
        const double*       collb,
        const double*       colub,
        const double*       obj,
        const double*       rowlb,
        const double*       rowub);

    // -------------------------------------------------------------------------

    // Read the quadratic part of the objective (the matrix part).
    void Clp_loadQuadraticObjective(
        Clp_Simplex*        model,
        int                 numberColumns,
        const CoinBigIndex* start,
        const int*          column,
        const double*       element);

    // -------------------------------------------------------------------------

    // Read an MPS file from the given `filename`.
    int Clp_readMps(
        Clp_Simplex* model,
        const char*  filename,
        int          keepNames,
        int          ignoreErrors);

    // -------------------------------------------------------------------------

    // Copy in integer information.
    void Clp_copyInIntegerInformation(
        Clp_Simplex* model,
        const char*  information);

    // Drop integer information.
    void Clp_deleteIntegerInformation(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Resizes the rim part of model.
    void Clp_resize(
        Clp_Simplex* model,
        int          newNumberRows,
        int          newNumberColumns);

    // -------------------------------------------------------------------------

    // Delete rows.
    void Clp_deleteRows(
        Clp_Simplex* model,
        int          number,
        const int*   which);

    // Add rows.
    void Clp_addRows(
        Clp_Simplex*  model,
        int           number,
        const double* rowLower,
        const double* rowUpper,
        const int*    rowStarts,
        const int*    columns,
        const double* elements);

    // -------------------------------------------------------------------------

    // Delete columns.
    void Clp_deleteColumns(
        Clp_Simplex* model,
        int          number,
        const int*   which);

    // Add columns.
    void Clp_addColumns(
        Clp_Simplex*  model,
        int           number,
        const double* columnLower,
        const double* columnUpper,
        const double* objective,
        const int*    columnStarts,
        const int*    rows,
        const double* elements);

    // -------------------------------------------------------------------------

    // Change the row lower bounds.
    void Clp_chgRowLower(
        Clp_Simplex*  model,
        const double* rowLower);

    // Change the row upper bounds.
    void Clp_chgRowUpper(
        Clp_Simplex*  model,
        const double* rowUpper);

    // -------------------------------------------------------------------------

    // Change the column lower bounds.
    void Clp_chgColumnLower(
        Clp_Simplex*  model,
        const double* columnLower);

    // Change the column upper bounds.
    void Clp_chgColumnUpper(
        Clp_Simplex*  model,
        const double* columnUpper);

    // -------------------------------------------------------------------------

    // Change the objective coefficients.
    void Clp_chgObjCoefficients(
        Clp_Simplex*  model,
        const double* objIn);

    // -------------------------------------------------------------------------

    // Drops names - makes lengthnames 0 and names empty
    void Clp_dropNames(
        Clp_Simplex* model);

    // Copies in names
    void Clp_copyNames(
        Clp_Simplex*       model,
        const char* const* rowNames,
        const char* const* columnNames);

    // -------------------------------------------------------------------------

    // Get the number of rows.
    int Clp_numberRows(
        Clp_Simplex* model);

    // Get the number of columns.
    int Clp_numberColumns(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the primal tolerance.
    void Clp_setPrimalTolerance(
        Clp_Simplex* model,
        double       value);

    // Get the primal tolerance.
    double Clp_primalTolerance(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the dual tolerance.
    void Clp_setDualTolerance(
        Clp_Simplex* model,
        double       value);

    // Get the dual tolerance.
    double Clp_dualTolerance(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the dual objective limit.
    void Clp_setDualObjectiveLimit(
        Clp_Simplex* model,
        double       value);

    // Get the dual objective limit.
    double Clp_dualObjectiveLimit(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the objective offset.
    void Clp_setObjectiveOffset(
        Clp_Simplex* model,
        double       value);

    // Get the objective offset.
    double Clp_objectiveOffset(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the problem name.
    // The given `array` must be `NUL`-terminated.
    int Clp_setProblemName(
        Clp_Simplex* model,
        int          maxNumberCharacters,
        char*        array);

    // Get the problem name.
    void Clp_problemName(
        Clp_Simplex* model,
        int          maxNumberCharacters,
        char*        array);

    // -------------------------------------------------------------------------

    // Set the number of iterations.
    void Clp_setNumberIterations(
        Clp_Simplex* model,
        int          numberIterations);

    // Get the number of iterations.
    int Clp_numberIterations(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the maximum number of iterations.
    void Clp_setMaximumIterations(
        Clp_Simplex* model,
        int          value);

    // Get the maximum number of iterations.
    int maximumIterations(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Maximum time in seconds (from when set called)
    void Clp_setMaximumSeconds(
        Clp_Simplex* model,
        double       value);

    // Maximum time in seconds (from when set called)
    double Clp_maximumSeconds(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Returns true if hit maximum iterations (or time).
    int Clp_hitMaximumIterations(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // The status of the problem.
    typedef enum {
        Clp_ProblemStatus_optimal             = 0,
        Clp_ProblemStatus_primalInfeasible    = 1,
        Clp_ProblemStatus_dualInfeasible      = 2,
        Clp_ProblemStatus_stoppedOnIterations = 3,
        Clp_ProblemStatus_stoppedByErrors     = 4,
    } Clp_ProblemStatus;

    // Set the status of the problem.
    void Clp_setProblemStatus(
        Clp_Simplex*      model,
        Clp_ProblemStatus problemStatus);

    // Get the status of the problem.
    Clp_ProblemStatus Clp_status(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // The secondary status of the problem.
    typedef enum {
        // There is no secondary status.
        Clp_SecondaryStatus_none                                 = 0,
        // Primal infeasible because dual limit reached.
        Clp_SecondaryStatus_dualLimitReached                     = 1,
        // Scaled is optimal; unscaled has primal infeasibilities.
        Clp_SecondaryStatus_unscaledPrimalInfeasibilities        = 2,
        // Scaled is optimal; unscaled has dual infeasibilities.
        Clp_SecondaryStatus_unscaledDualInfeasibilities          = 3,
        // Scaled is optimal; unscaled has primal and dual infeasibilities.
        Clp_SecondaryStatus_unscaledPrimalAndDualInfeasibilities = 4,
    } Clp_SecondaryStatus;

    // Set the secondary status of the problem.
    void Clp_setSecondaryStatus(
        Clp_Simplex*        model,
        Clp_SecondaryStatus status);

    // Get the secondary status of the problem.
    Clp_SecondaryStatus Clp_secondaryStatus(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    enum {
        Clp_OptimizationDir_minimize = 1.0,
        Clp_OptimizationDir_maximize = -1.0,
        Clp_OptimizationDir_ignore   = 0.0,
    };

    typedef double Clp_OptimizationDir;

    // Set the direction of optimization.
    void Clp_setOptimizationDirection(
        Clp_Simplex*        model,
        Clp_OptimizationDir value);

    // Get the direction of optimization.
    Clp_OptimizationDir Clp_optimizationDirection(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the primal row solution.
    double* Clp_primalRowSolution(
        Clp_Simplex* model);

    // Get the primal column solution.
    double* Clp_primalColumnSolution(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the dual row solution.
    double* Clp_dualRowSolution(
        Clp_Simplex* model);

    // Get the dual column solution.
    double* Clp_dualColumnSolution(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the row lower bound.
    double* Clp_rowLower(
        Clp_Simplex* model);

    // Get the row upper bound.
    double* Clp_rowUpper(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the objective.
    double* Clp_objective(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the column lower bound.
    double* Clp_columnLower(
        Clp_Simplex* model);

    // Get the column upper bound.
    double* Clp_columnUpper(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the number of elements in the matrix.
    int Clp_getNumElements(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the column starts of the matrix.
    const CoinBigIndex* Clp_getVectorStarts(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the row indices in the matrix.
    const int* Clp_getIndices(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the column vector lengths in the matrix.
    const int* Clp_getVectorLengths(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the element values of the matrix.
    const double* Clp_getElements(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the objective value.
    double Clp_objectiveValue(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the integer information.
    char* Clp_integerInformation(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    typedef double* Clp_Ray;

    // Gives the infeasibility ray.
    //
    // Use `Clp_freeRay` to free the returned array.
    //
    // @return The infeasibility ray, or `NULL` if none / wrong.
    Clp_Ray Clp_infeasibilityRay(
        Clp_Simplex* model);

    // Gives the ray in which the problem is unbounded.
    //
    // Use `Clp_freeRay` to free the returned array.
    //
    // @return unbounded ray, or `NULL` if none / wrong.
    Clp_Ray Clp_unboundedRay(
        Clp_Simplex* model);

    // Frees an infeasibility or unbounded ray.
    void Clp_freeRay(
        Clp_Simplex* model,
        Clp_Ray      ray);

    // -------------------------------------------------------------------------

    // As defined in `ClpSimplex.hpp`.
    enum {
        Clp_Status_free       = 0,
        Clp_Status_basic      = 1,
        Clp_Status_atUpper    = 2,
        Clp_Status_atLower    = 3,
        Clp_Status_superbasic = 4,
        Clp_Status_fixed      = 5,
    };

    typedef unsigned char Clp_Status;

    // See if status array exists (partly for `OsiClp`).
    int Clp_statusExists(
        Clp_Simplex* model);

    // Return address of status array `(char[numberRows + numberColumns])`
    Clp_Status* Clp_statusArray(
        Clp_Simplex* model);

    // Copy in the status vector.
    void Clp_copyinStatus(
        Clp_Simplex*      model,
        const Clp_Status* statusArray);

    // -------------------------------------------------------------------------

    // Set variable basis info (and value if at bound).
    void Clp_setColumnStatus(
        Clp_Simplex* model,
        int          sequence,
        int          value);

    // Get variable basis info.
    int Clp_getColumnStatus(
        Clp_Simplex* model,
        int          sequence);

    // -------------------------------------------------------------------------

    // Set row basis info (and value if at bound).
    void Clp_setRowStatus(
        Clp_Simplex* model,
        int          sequence,
        int          value);

    // Get row basis info.
    int Clp_getRowStatus(
        Clp_Simplex* model,
        int          sequence);

    // -------------------------------------------------------------------------

    typedef void* Clp_UserPointer;

    // Set the user pointer.
    void Clp_setUserPointer(
        Clp_Simplex*    model,
        Clp_UserPointer pointer);

    // Get the user pointer.
    Clp_UserPointer Clp_getUserPointer(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Pass in the callback function.
    // Message numbers up to `1000000` are Clp, Coin ones have `1000000` added.
    void Clp_registerCallBack(
        Clp_Simplex* model,
        clp_callback userCallBack);

    // Unset the callback function.
    void Clp_clearCallBack(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    typedef enum {
        Clp_LogLevel_none                  = 0,
        Clp_LogLevel_justFinal             = 1,
        Clp_LogLevel_justFactorizations    = 2,
        Clp_LogLevel_factorizationsAndMore = 3,
        Clp_LogLevel_verbose               = 4,
        Clp_LogLevel_selectiveDebug1       = 8,
        Clp_LogLevel_selectiveDebug2       = 16,
        Clp_LogLevel_selectiveDebug3       = 32,
    } Clp_LogLevel;

    // Set the log level.
    void Clp_setLogLevel(
        Clp_Simplex* model,
        Clp_LogLevel value);

    // Get the log level.
    Clp_LogLevel Clp_logLevel(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the length of names (if `0` is returned it means there are no names).
    int Clp_lengthNames(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Fill in array (at least `lengthNames + 1` long) with a row name.
    void Clp_rowName(
        Clp_Simplex* model,
        int          iRow,
        char*        name);

    // Fill in array (at least `lengthNames + 1` long) with a column name.
    void Clp_columnName(
        Clp_Simplex* model,
        int          iColumn,
        char*        name);

    // -------------------------------------------------------------------------

    // General solve algorithm which can do presolve.
    // See `ClpSolve.hpp` for options.
    int Clp_initialSolve(
        Clp_Simplex* model);

    // Pass solve options. (exception to direct analogue rule)
    int Clp_initialSolveWithOptions(
        Clp_Simplex* model,
        Clp_Solve*   solve);

    // Dual initial solve
    int Clp_initialDualSolve(
        Clp_Simplex* model);

    // Primal initial solve
    int Clp_initialPrimalSolve(
        Clp_Simplex* model);

    // Barrier initial solve
    int Clp_initialBarrierSolve(
        Clp_Simplex* model);

    // Barrier initial solve, no crossover
    int Clp_initialBarrierNoCrossSolve(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Dual algorithm - see `ClpSimplexDual.hpp` for method
    int Clp_dual(
        Clp_Simplex* model,
        int          ifValuesPass);

    // -------------------------------------------------------------------------

    // Primal algorithm - see `ClpSimplexPrimal.hpp` for method
    int Clp_primal(
        Clp_Simplex* model,
        int          ifValuesPass);

    // -------------------------------------------------------------------------

#ifndef SLIM_CLP
    // Solve the problem with the idiot code
    void Clp_idiot(
        Clp_Simplex* model,
        int          tryhard);
#endif

    // -------------------------------------------------------------------------

    typedef enum {
        Clp_Scaling_off         = 0, // scaling off
        Clp_Scaling_equilibrium = 1, // equilibrium scaling
        Clp_Scaling_geometric   = 2, // geometric scaling
        Clp_Scaling_auto        = 3, // choose a scaling mode automatically
        Clp_Scaling_dynamic     = 4  // dynamic scaling (later)
    } Clp_Scaling;

    // Set the scaling mode.
    void Clp_scaling(
        Clp_Simplex* model,
        Clp_Scaling  mode);

    // Get the scaling mode.
    Clp_Scaling Clp_scalingFlag(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Crash: at present just aimed at dual, returns:
    //     -2 - if dual preferred and crash basis created
    //     -1 - if dual preferred and all slack basis preferred
    //     0  - if basis going in was not all slack
    //     1  - if primal preferred and all slack basis preferred
    //     2  - if primal preferred and crash basis created.
    //
    // If the gap between bounds is less than or equal to `gap`, variables
    // can be flipped.
    //
    // If `pivot` is
    //     0 - No pivoting (so will just be choice of algorithm).
    //     1 - Simple pivoting, e.g.: GUB.
    //     2 - Mini iterations
    int Clp_crash(
        Clp_Simplex* model,
        double       gap,
        int          pivot);

    // -------------------------------------------------------------------------

    // Is the problem is primal feasible?
    int Clp_primalFeasible(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Is the problem dual feasible?
    int Clp_dualFeasible(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the dual bound.
    void Clp_setDualBound(
        Clp_Simplex* model,
        double       value);

    // Get the dual bound.
    double Clp_dualBound(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the infeasibility cost.
    void Clp_setInfeasibilityCost(
        Clp_Simplex* model,
        double       value);

    // Get the infeasibility cost.
    double Clp_infeasibilityCost(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    typedef enum {
        // Switch on perturbation.
        Clp_Perturbation_switchOnPerturbation = 50,
        // Auto perturb if it takes too long (1e-6 largest nonzero).
        // This is typically the default.
        Clp_Perturbation_autoPerturb          = 100,
        // We are perturbed.
        Clp_Perturbation_perturbed            = 101,
        // Don't try perturbing again.
        Clp_Perturbation_dontPerturbAgain     = 102,
    } Clp_Perturbation;

    // Get the perturbation mode.
    // The default value is `Clp_Perturbation_autoPerturb`.
    // The others are for playing.
    Clp_Perturbation Clp_perturbation(
        Clp_Simplex* model);

    // Set the perturbation mode.
    // The default value is `Clp_Perturbation_autoPerturb`.
    // The others are for playing.
    void Clp_setPerturbation(
        Clp_Simplex*     model,
        Clp_Perturbation value);

    // -------------------------------------------------------------------------

    // Set the algorithm to use.
    void Clp_setAlgorithm(
        Clp_Simplex* model,
        int          value);

    // Get the algorithm to use.
    int Clp_algorithm(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the sum of the dual infeasibilities.
    double Clp_sumDualInfeasibilities(
        Clp_Simplex* model);

    // Get the number of dual infeasibilities.
    int Clp_numberDualInfeasibilities(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the sum of the primal infeasibilities.
    double Clp_sumPrimalInfeasibilities(
        Clp_Simplex* model);

    // Get the number of primal infeasibilities.
    int Clp_numberPrimalInfeasibilities(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Save model to file, returns `0` if success.
    // This is designed for use outside algorithms so does not save iterating
    // arrays etc.
    // This function does not save any messaging information or scaling values.
    // It does not know about all types of virtual functions.
    int Clp_saveModel(
        Clp_Simplex* model,
        const char*  fileName);

    // Restore model from file, returns `0` if success, deletes current model.
    int Clp_restoreModel(
        Clp_Simplex* model,
        const char*  fileName);

    // -------------------------------------------------------------------------

    // Just check solution (for external use) -
    // sets sum of infeasibilities, etc.
    void Clp_checkSolution(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Number of rows
    int Clp_getNumRows(
        Clp_Simplex* model);

    // Number of columns
    int Clp_getNumCols(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Number of iterations
    int Clp_getIterationCount(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Are there a numerical difficulties?
    int Clp_isAbandoned(
        Clp_Simplex* model);

    // Is optimality proven?
    int Clp_isProvenOptimal(
        Clp_Simplex* model);

    // Is primal infeasiblity proven?
    int Clp_isProvenPrimalInfeasible(
        Clp_Simplex* model);

    // Is dual infeasiblity proven?
    int Clp_isProvenDualInfeasible(
        Clp_Simplex* model);

    // Is the given primal objective limit reached?
    int Clp_isPrimalObjectiveLimitReached(
        Clp_Simplex* model);

    // Is the given dual objective limit reached?
    int Clp_isDualObjectiveLimitReached(
        Clp_Simplex* model);

    // Iteration limit reached?
    int Clp_isIterationLimitReached(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the direction of optimization.
    void Clp_setObjSense(
        Clp_Simplex*        model,
        Clp_OptimizationDir objsen);

    // Get the direction of optimization.
    Clp_OptimizationDir Clp_getObjSense(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the primal row solution.
    const double* Clp_getRowActivity(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set the primal column solution.
    void Clp_setColSolution(
        Clp_Simplex*  model,
        const double* input);

    // Get the primal column solution.
    const double* Clp_getColSolution(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the dual row solution.
    const double* Clp_getRowPrice(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the reduced costs.
    const double* Clp_getReducedCost(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the row lower bounds.
    const double* Clp_getRowLower(
        Clp_Simplex* model);

    // Get the row upper bounds.
    const double* Clp_getRowUpper(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the objective coefficients.
    const double* Clp_getObjCoefficients(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the column lower bounds.
    const double* Clp_getColLower(
        Clp_Simplex* model);

    // Get the column upper bounds.
    const double* Clp_getColUpper(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Get the objective value.
    double Clp_getObjValue(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Print the model for debugging purposes.
    void Clp_printModel(
        Clp_Simplex* model,
        const char*  prefix);

    // -------------------------------------------------------------------------

    const double CLP_DEFAULT_SMALL_ELEMENT_VALUE = 1.0e-20;

    // Set the small element value.
    // Elements less than the small element value are set to zero.
    // The default SEV is `CLP_DEFAULT_SMALL_ELEMENT_VALUE`.
    void Clp_setSmallElementValue(
        Clp_Simplex* model,
        double       value);

    // Get the small element value.
    // Elements less than the small element value are set to zero.
    // The default SEV is `CLP_DEFAULT_SMALL_ELEMENT_VALUE`.
    double Clp_getSmallElementValue(
        Clp_Simplex* model);

    // -------------------------------------------------------------------------

    // Set `ClpSolve` special options.
    void ClpSolve_setSpecialOption(
        Clp_Solve* solve,
        int        which,
        int        value,
        int        extraInfo);

    // Get `ClpSolve` special options.
    int ClpSolve_getSpecialOption(
        Clp_Solve* solve,
        int        which);

    // -------------------------------------------------------------------------

    // See `ClpSolve::SolveType`.
    typedef enum {
        Clp_SolveType_useDual           = 0, // dual simplex
        Clp_SolveType_usePrimal         = 1, // primal simplex
        Clp_SolveType_usePrimalorSprint = 2, // primal or sprint
        Clp_SolveType_useBarrier        = 3, // barrier
        Clp_SolveType_useBarrierNoCross = 4, // barrier no crossover
        Clp_SolveType_automatic         = 5, // automatic
        Clp_SolveType_notImplemented    = 6  // not implemented
    } Clp_SolveType;

    // Set the solver behavior.
    // Pass `-1` for `extraInfo` to get default behavior.
    void ClpSolve_setSolveType(
        Clp_Solve*    solve,
        Clp_SolveType method,
        int           extraInfo);

    // Get the solver behavior.
    Clp_SolveType ClpSolve_getSolveType(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // See `ClpSolve::PresolveType`.
    typedef enum {
        Clp_PresolveType_presolveOn         = 0, // presolve on
        Clp_PresolveType_presolveOff        = 1, // presolve off
        Clp_PresolveType_presolveNumber     = 2, // presolve number
        Clp_PresolveType_presolveNumberCost = 3  // presolve number cost
    } Clp_PresolveType;

    // Set the presolver behavior.
    // Pass `-1` for `extraInfo` to get default behavior.
    void ClpSolve_setPresolveType(
        Clp_Solve*       solve,
        Clp_PresolveType amount,
        int              extraInfo);

    // Get the presolver behavior.
    Clp_PresolveType ClpSolve_getPresolveType(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    int ClpSolve_getPresolvePasses(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    int ClpSolve_getExtraInfo(
        Clp_Solve* solve,
        int        which);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setInfeasibleReturn(
        Clp_Solve* solve,
        int        trueFalse);

    // FIXME: doc
    int ClpSolve_infeasibleReturn(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoDual(
        Clp_Solve* solve,
        int        doDual);

    // FIXME: doc
    int ClpSolve_doDual(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoSingleton(
        Clp_Solve* solve,
        int        doSingleton);

    // FIXME: doc
    int ClpSolve_doSingleton(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoDoubleton(
        Clp_Solve* solve,
        int        doDoubleton);

    // FIXME: doc
    int ClpSolve_doDoubleton(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoTripleton(
        Clp_Solve* solve,
        int        doTripleton);

    // FIXME: doc
    int ClpSolve_doTripleton(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoTighten(
        Clp_Solve* solve,
        int        doTighten);

    // FIXME: doc
    int ClpSolve_doTighten(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoForcing(
        Clp_Solve* solve,
        int        doForcing);

    // FIXME: doc
    int ClpSolve_doForcing(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoImpliedFree(
        Clp_Solve* solve,
        int        doImpliedFree);

    // FIXME: doc
    int ClpSolve_doImpliedFree(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoDupcol(
        Clp_Solve* solve,
        int        doDupcol);

    // FIXME: doc
    int ClpSolve_doDupcol(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoDuprow(
        Clp_Solve* solve,
        int        doDuprow);

    // FIXME: doc
    int ClpSolve_doDuprow(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setDoSingletonColumn(
        Clp_Solve* solve,
        int        doSingleton);

    // FIXME: doc
    int ClpSolve_doSingletonColumn(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setPresolveActions(
        Clp_Solve* solve,
        int        action);

    // FIXME: doc
    int ClpSolve_presolveActions(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

    // FIXME: doc
    void ClpSolve_setSubstitution(
        Clp_Solve* solve,
        int        value);

    // FIXME: doc
    int ClpSolve_substitution(
        Clp_Solve* solve);

    // -------------------------------------------------------------------------

#ifdef __cplusplus
}
#endif
#endif
