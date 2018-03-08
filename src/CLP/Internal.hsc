{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
#include <bindings.dsl.h>
#include "../../../include/clp.h"
module CLP.Internal where
import Foreign.Ptr
#strict_import

#opaque_t Clp_Solve
#opaque_t Clp_Simplex

#ccall Clp_Version , IO CString
#ccall Clp_VersionMajor , IO CInt
#ccall Clp_VersionMinor , IO CInt
#ccall Clp_VersionRelease , IO CInt
#ccall Clp_newModel , IO (Ptr <Clp_Simplex>)
#ccall Clp_deleteModel , Ptr <Clp_Simplex> -> IO ()
#ccall ClpSolve_new , IO (Ptr <Clp_Solve>)
#ccall ClpSolve_delete , Ptr <Clp_Solve> -> IO ()
#ccall Clp_loadProblem , Ptr <Clp_Simplex> -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
#ccall Clp_loadQuadraticObjective , Ptr <Clp_Simplex> -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
#ccall Clp_readMps , Ptr <Clp_Simplex> -> CString -> CInt -> CInt -> IO CInt
#ccall Clp_copyInIntegerInformation , Ptr <Clp_Simplex> -> CString -> IO ()
#ccall Clp_deleteIntegerInformation , Ptr <Clp_Simplex> -> IO ()
#ccall Clp_resize , Ptr <Clp_Simplex> -> CInt -> CInt -> IO ()
#ccall Clp_deleteRows , Ptr <Clp_Simplex> -> CInt -> Ptr CInt -> IO ()
#ccall Clp_addRows , Ptr <Clp_Simplex> -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
#ccall Clp_deleteColumns , Ptr <Clp_Simplex> -> CInt -> Ptr CInt -> IO ()
#ccall Clp_addColumns , Ptr <Clp_Simplex> -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
#ccall Clp_chgRowLower , Ptr <Clp_Simplex> -> Ptr CDouble -> IO ()
#ccall Clp_chgRowUpper , Ptr <Clp_Simplex> -> Ptr CDouble -> IO ()
#ccall Clp_chgColumnLower , Ptr <Clp_Simplex> -> Ptr CDouble -> IO ()
#ccall Clp_chgColumnUpper , Ptr <Clp_Simplex> -> Ptr CDouble -> IO ()
#ccall Clp_chgObjCoefficients , Ptr <Clp_Simplex> -> Ptr CDouble -> IO ()
#ccall Clp_dropNames , Ptr <Clp_Simplex> -> IO ()
#ccall Clp_copyNames , Ptr <Clp_Simplex> -> Ptr CString -> Ptr CString -> IO ()
#ccall Clp_numberRows , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_numberColumns , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_setPrimalTolerance , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_primalTolerance , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_setDualTolerance , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_dualTolerance , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_setDualObjectiveLimit , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_dualObjectiveLimit , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_setObjectiveOffset , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_objectiveOffset , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_setProblemName , Ptr <Clp_Simplex> -> CInt -> CString -> IO CInt
#ccall Clp_problemName , Ptr <Clp_Simplex> -> CInt -> CString -> IO ()
#ccall Clp_setNumberIterations , Ptr <Clp_Simplex> -> CInt -> IO ()
#ccall Clp_numberIterations , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_setMaximumIterations , Ptr <Clp_Simplex> -> CInt -> IO ()
#ccall maximumIterations , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_setMaximumSeconds , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_maximumSeconds , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_hitMaximumIterations , Ptr <Clp_Simplex> -> IO CInt
{- typedef enum {
            Clp_ProblemStatus_optimal = 0,
            Clp_ProblemStatus_primalInfeasible = 1,
            Clp_ProblemStatus_dualInfeasible = 2,
            Clp_ProblemStatus_stoppedOnIterations = 3,
            Clp_ProblemStatus_stoppedByErrors = 4
        } Clp_ProblemStatus; -}
#integral_t Clp_ProblemStatus
#num_pattern Clp_ProblemStatus_optimal
#num_pattern Clp_ProblemStatus_primalInfeasible
#num_pattern Clp_ProblemStatus_dualInfeasible
#num_pattern Clp_ProblemStatus_stoppedOnIterations
#num_pattern Clp_ProblemStatus_stoppedByErrors
#ccall Clp_setProblemStatus , Ptr <Clp_Simplex> -> <Clp_ProblemStatus> -> IO ()
#ccall Clp_status , Ptr <Clp_Simplex> -> IO <Clp_ProblemStatus>
{- typedef enum {
            Clp_SecondaryStatus_none = 0,
            Clp_SecondaryStatus_dualLimitReached = 1,
            Clp_SecondaryStatus_unscaledPrimalInfeasibilities = 2,
            Clp_SecondaryStatus_unscaledDualInfeasibilities = 3,
            Clp_SecondaryStatus_unscaledPrimalAndDualInfeasibilities = 4
        } Clp_SecondaryStatus; -}
#integral_t Clp_SecondaryStatus
#num_pattern Clp_SecondaryStatus_none
#num_pattern Clp_SecondaryStatus_dualLimitReached
#num_pattern Clp_SecondaryStatus_unscaledPrimalInfeasibilities
#num_pattern Clp_SecondaryStatus_unscaledDualInfeasibilities
#num_pattern Clp_SecondaryStatus_unscaledPrimalAndDualInfeasibilities
#ccall Clp_setSecondaryStatus , Ptr <Clp_Simplex> -> <Clp_SecondaryStatus> -> IO ()
#ccall Clp_secondaryStatus , Ptr <Clp_Simplex> -> IO <Clp_SecondaryStatus>
{- typedef double Clp_OptimizationDir; -}
#fractional_pattern Clp_OptimizationDir_minimize
#fractional_pattern Clp_OptimizationDir_maximize
#fractional_pattern Clp_OptimizationDir_ignore
#synonym_t Clp_OptimizationDir , CDouble
#ccall Clp_setOptimizationDirection , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_optimizationDirection , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_primalRowSolution , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_primalColumnSolution , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_dualRowSolution , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_dualColumnSolution , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_rowLower , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_rowUpper , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_objective , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_columnLower , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_columnUpper , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_getNumElements , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_getVectorStarts , Ptr <Clp_Simplex> -> IO (Ptr CInt)
#ccall Clp_getIndices , Ptr <Clp_Simplex> -> IO (Ptr CInt)
#ccall Clp_getVectorLengths , Ptr <Clp_Simplex> -> IO (Ptr CInt)
#ccall Clp_getElements , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_objectiveValue , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_integerInformation , Ptr <Clp_Simplex> -> IO CString
#synonym_t Clp_Ray , Ptr CDouble
#ccall Clp_infeasibilityRay , Ptr <Clp_Simplex> -> IO <Clp_Ray>
#ccall Clp_unboundedRay , Ptr <Clp_Simplex> -> IO <Clp_Ray>
#ccall Clp_freeRay , Ptr <Clp_Simplex> -> <Clp_Ray> -> IO ()
{- enum {
    Clp_Status_free = 0,
    Clp_Status_basic = 1,
    Clp_Status_atUpper = 2,
    Clp_Status_atLower = 3,
    Clp_Status_superbasic = 4,
    Clp_Status_fixed = 5
}; -}
#num_pattern Clp_Status_free
#num_pattern Clp_Status_basic
#num_pattern Clp_Status_atUpper
#num_pattern Clp_Status_atLower
#num_pattern Clp_Status_superbasic
#num_pattern Clp_Status_fixed
{- typedef unsigned char Clp_Status; -}
#synonym_t Clp_Status , CUChar
#ccall Clp_statusExists , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_statusArray , Ptr <Clp_Simplex> -> IO (Ptr CUChar)
#ccall Clp_copyinStatus , Ptr <Clp_Simplex> -> Ptr CUChar -> IO ()
#ccall Clp_setColumnStatus , Ptr <Clp_Simplex> -> CInt -> CInt -> IO ()
#ccall Clp_getColumnStatus , Ptr <Clp_Simplex> -> CInt -> IO CInt
#ccall Clp_setRowStatus , Ptr <Clp_Simplex> -> CInt -> CInt -> IO ()
#ccall Clp_getRowStatus , Ptr <Clp_Simplex> -> CInt -> IO CInt
#callback clp_callback , Ptr <Clp_Simplex> -> CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> CInt -> Ptr CString -> IO ()
#synonym_t Clp_UserPointer , Ptr ()
#ccall Clp_setUserPointer , Ptr <Clp_Simplex> -> <Clp_UserPointer> -> IO ()
#ccall Clp_getUserPointer , Ptr <Clp_Simplex> -> IO <Clp_UserPointer>
#ccall Clp_registerCallBack , Ptr <Clp_Simplex> -> <clp_callback> -> IO ()
#ccall Clp_clearCallBack , Ptr <Clp_Simplex> -> IO ()
{- typedef enum {
            Clp_LogLevel_none = 0,
            Clp_LogLevel_justFinal = 1,
            Clp_LogLevel_justFactorizations = 2,
            Clp_LogLevel_factorizationsAndMore = 3,
            Clp_LogLevel_verbose = 4,
            Clp_LogLevel_selectiveDebug1 = 8,
            Clp_LogLevel_selectiveDebug2 = 16,
            Clp_LogLevel_selectiveDebug3 = 32
        } Clp_LogLevel; -}
#integral_t Clp_LogLevel
#num_pattern Clp_LogLevel_none
#num_pattern Clp_LogLevel_justFinal
#num_pattern Clp_LogLevel_justFactorizations
#num_pattern Clp_LogLevel_factorizationsAndMore
#num_pattern Clp_LogLevel_verbose
#num_pattern Clp_LogLevel_selectiveDebug1
#num_pattern Clp_LogLevel_selectiveDebug2
#num_pattern Clp_LogLevel_selectiveDebug3
#ccall Clp_setLogLevel , Ptr <Clp_Simplex> -> <Clp_LogLevel> -> IO ()
#ccall Clp_logLevel , Ptr <Clp_Simplex> -> IO <Clp_LogLevel>
#ccall Clp_lengthNames , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_rowName , Ptr <Clp_Simplex> -> CInt -> CString -> IO ()
#ccall Clp_columnName , Ptr <Clp_Simplex> -> CInt -> CString -> IO ()
#ccall Clp_initialSolve , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_initialSolveWithOptions , Ptr <Clp_Simplex> -> Ptr <Clp_Solve> -> IO CInt
#ccall Clp_initialDualSolve , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_initialPrimalSolve , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_initialBarrierSolve , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_initialBarrierNoCrossSolve , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_dual , Ptr <Clp_Simplex> -> CInt -> IO CInt
#ccall Clp_primal , Ptr <Clp_Simplex> -> CInt -> IO CInt
#ccall Clp_idiot , Ptr <Clp_Simplex> -> CInt -> IO ()
{- typedef enum {
            Clp_Scaling_off = 0,
            Clp_Scaling_equilibrium = 1,
            Clp_Scaling_geometric = 2,
            Clp_Scaling_auto = 3,
            Clp_Scaling_dynamic = 4
        } Clp_Scaling; -}
#integral_t Clp_Scaling
#num_pattern Clp_Scaling_off
#num_pattern Clp_Scaling_equilibrium
#num_pattern Clp_Scaling_geometric
#num_pattern Clp_Scaling_auto
#num_pattern Clp_Scaling_dynamic
#ccall Clp_scaling , Ptr <Clp_Simplex> -> <Clp_Scaling> -> IO ()
#ccall Clp_scalingFlag , Ptr <Clp_Simplex> -> IO <Clp_Scaling>
#ccall Clp_crash , Ptr <Clp_Simplex> -> CDouble -> CInt -> IO CInt
#ccall Clp_primalFeasible , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_dualFeasible , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_setDualBound , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_dualBound , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_setInfeasibilityCost , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_infeasibilityCost , Ptr <Clp_Simplex> -> IO CDouble
{- typedef enum {
            Clp_Perturbation_switchOnPerturbation = 50,
            Clp_Perturbation_autoPerturb = 100,
            Clp_Perturbation_perturbed = 101,
            Clp_Perturbation_dontPerturbAgain = 102
        } Clp_Perturbation; -}
#integral_t Clp_Perturbation
#num_pattern Clp_Perturbation_switchOnPerturbation
#num_pattern Clp_Perturbation_autoPerturb
#num_pattern Clp_Perturbation_perturbed
#num_pattern Clp_Perturbation_dontPerturbAgain
#ccall Clp_perturbation , Ptr <Clp_Simplex> -> IO <Clp_Perturbation>
#ccall Clp_setPerturbation , Ptr <Clp_Simplex> -> <Clp_Perturbation> -> IO ()
#ccall Clp_setAlgorithm , Ptr <Clp_Simplex> -> CInt -> IO ()
#ccall Clp_algorithm , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_sumDualInfeasibilities , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_numberDualInfeasibilities , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_sumPrimalInfeasibilities , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_numberPrimalInfeasibilities , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_saveModel , Ptr <Clp_Simplex> -> CString -> IO CInt
#ccall Clp_restoreModel , Ptr <Clp_Simplex> -> CString -> IO CInt
#ccall Clp_checkSolution , Ptr <Clp_Simplex> -> IO ()
#ccall Clp_getNumRows , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_getNumCols , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_getIterationCount , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_isAbandoned , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_isProvenOptimal , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_isProvenPrimalInfeasible , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_isProvenDualInfeasible , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_isPrimalObjectiveLimitReached , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_isDualObjectiveLimitReached , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_isIterationLimitReached , Ptr <Clp_Simplex> -> IO CInt
#ccall Clp_setObjSense , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_getObjSense , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_getRowActivity , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_setColSolution , Ptr <Clp_Simplex> -> Ptr CDouble -> IO ()
#ccall Clp_getColSolution , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_getRowPrice , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_getReducedCost , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_getRowLower , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_getRowUpper , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_getObjCoefficients , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_getColLower , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_getColUpper , Ptr <Clp_Simplex> -> IO (Ptr CDouble)
#ccall Clp_getObjValue , Ptr <Clp_Simplex> -> IO CDouble
#ccall Clp_printModel , Ptr <Clp_Simplex> -> CString -> IO ()
#globalvar CLP_DEFAULT_SMALL_ELEMENT_VALUE , CDouble
#ccall Clp_setSmallElementValue , Ptr <Clp_Simplex> -> CDouble -> IO ()
#ccall Clp_getSmallElementValue , Ptr <Clp_Simplex> -> IO CDouble
#ccall ClpSolve_setSpecialOption , Ptr <Clp_Solve> -> CInt -> CInt -> CInt -> IO ()
#ccall ClpSolve_getSpecialOption , Ptr <Clp_Solve> -> CInt -> IO CInt
{- typedef enum {
            Clp_SolveType_useDual = 0,
            Clp_SolveType_usePrimal = 1,
            Clp_SolveType_usePrimalorSprint = 2,
            Clp_SolveType_useBarrier = 3,
            Clp_SolveType_useBarrierNoCross = 4,
            Clp_SolveType_automatic = 5,
            Clp_SolveType_notImplemented = 6
        } Clp_SolveType; -}
#integral_t Clp_SolveType
#num_pattern Clp_SolveType_useDual
#num_pattern Clp_SolveType_usePrimal
#num_pattern Clp_SolveType_usePrimalorSprint
#num_pattern Clp_SolveType_useBarrier
#num_pattern Clp_SolveType_useBarrierNoCross
#num_pattern Clp_SolveType_automatic
#num_pattern Clp_SolveType_notImplemented
#ccall ClpSolve_setSolveType , Ptr <Clp_Solve> -> <Clp_SolveType> -> CInt -> IO ()
#ccall ClpSolve_getSolveType , Ptr <Clp_Solve> -> IO <Clp_SolveType>
{- typedef enum {
            Clp_PresolveType_presolveOn = 0,
            Clp_PresolveType_presolveOff = 1,
            Clp_PresolveType_presolveNumber = 2,
            Clp_PresolveType_presolveNumberCost = 3
        } Clp_PresolveType; -}
#integral_t Clp_PresolveType
#num_pattern Clp_PresolveType_presolveOn
#num_pattern Clp_PresolveType_presolveOff
#num_pattern Clp_PresolveType_presolveNumber
#num_pattern Clp_PresolveType_presolveNumberCost
#ccall ClpSolve_setPresolveType , Ptr <Clp_Solve> -> <Clp_PresolveType> -> CInt -> IO ()
#ccall ClpSolve_getPresolveType , Ptr <Clp_Solve> -> IO <Clp_PresolveType>
#ccall ClpSolve_getPresolvePasses , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_getExtraInfo , Ptr <Clp_Solve> -> CInt -> IO CInt
#ccall ClpSolve_setInfeasibleReturn , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_infeasibleReturn , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoDual , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doDual , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoSingleton , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doSingleton , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoDoubleton , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doDoubleton , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoTripleton , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doTripleton , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoTighten , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doTighten , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoForcing , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doForcing , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoImpliedFree , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doImpliedFree , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoDupcol , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doDupcol , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoDuprow , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doDuprow , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setDoSingletonColumn , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_doSingletonColumn , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setPresolveActions , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_presolveActions , Ptr <Clp_Solve> -> IO CInt
#ccall ClpSolve_setSubstitution , Ptr <Clp_Solve> -> CInt -> IO ()
#ccall ClpSolve_substitution , Ptr <Clp_Solve> -> IO CInt
