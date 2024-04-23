#include <Rcpp.h>
using namespace Rcpp;

//' Wealth transmission from year T-1 to T
//' 
//' Take last imputed year (T-1) and transmit
//'  wealth to year T given household change
//'  
//' @details Among the most remarkable rules:
//' \itemize{
//' \item{\emph{Divorce}: Household wealth divided between spouses}
//' \item{\emph{Marriage}: Household wealth merged between spouses}
//' }
//' 
//' @inheritParams assign_referent_cpp
//' @param patrimoine Wealth that has been
//'  imputed in a previous iteration 
//'  or that should be imputed before next
//'  iteration
//' @param year Year vector
//' @param matri_status Matrimonious or family status. \emph{1}: leave parents ;
//'  \emph{2}: couple ; \emph{3}: husband/spouse death ; \emph{4}: divorce
//' @param spouse_id Husband/Spouse identifier. Missing values if no
//'  husband/spouse
//' @param patrimoine_conjoint Husband/Spouse wealth
//' @param matri_conjoint Husband/Spouse matrimonious or family status. \emph{1}: leave parents ;
//'  \emph{2}: couple ; \emph{3}: husband/spouse death ; \emph{4}: divorce
//' @param year_loop T: loop year inside \code{inherit_wealth} is launched
//' @param referent_list List of household head in T-1
//'  
//' @return List: K[T-1], K[T] after demographic change
//' @export
//[[Rcpp::export]]
List inherit_wealth(NumericVector id,
                    NumericVector patrimoine,
                    NumericVector year,
                    NumericVector matri_status,
                    NumericVector sexe,
                    NumericVector spouse_id,
                    NumericVector patrimoine_conjoint,
                    NumericVector matri_conjoint,
                    int year_loop,
                    NumericVector referent_list){
  
  
  // if ((year[0] != year_loop-1) & (year_loop>2009)){
  //   List x ;
  //   x["Kprev"] = 0;
  //   x["patrimoine"] = 0 ;
  //   x["standard_niveau"] = 0;
  //   x["standard_patrimoine"] = 0;
  //   return x;
  // }
  
  
  // ========================================
  //                PART 0     
  //        PARAMETERS INITIALIZATION
  // ========================================
  
  
  // WEALTH TRANSMITTED FROM T-1 TO T  
  double inherit = 0;
  
  
  // OTHERS ELEMENTS
  int identifiant = id[0];
  bool referent_last_year;
  int start=0;

  
  // INDIVIDUAL BELONG TO T-1 HOUSEHOLD HEAD LIST ?
  if (std::find(referent_list.begin(), referent_list.end(), identifiant) != referent_list.end()){
    referent_last_year = true;
    start +=1;
  } else{
    referent_last_year = false;
  }

  // VALUES WHERE NOT CONSISTENT FOR FEMALES WHOSE HUSBAND DIE OR THAT DIVORCED
  if ((start==0) & (sexe[start]==2) & ((matri_status[1]==3)| (matri_status[1]==4))) start +=1;
  
  
  
  // ========================================
  //                PART 1     
  //           PEOPLE THAT DIE
  // ========================================
  
  // DON'T TRANSMIT WEALTH TO CHILDREN
  // --> POSSIBLE EXTENSION
  
  if (id.size()==1){
    List x ;
    x["Kprev"] = patrimoine[0] ;
    x["patrimoine"] = patrimoine[0] ;
    return x;
  }
  
  
  // ========================================
  //                PART 2     
  //           WEALTH TRANSMISSION
  // ========================================
  
  
  
  // --------------------------------------
  // CAS 1: DEPART DE CHEZ LES PARENTS
  // --------------------------------------
  
  
  if (matri_status[start]==1) inherit += 0;
  
  
  // --------------------------------------
  // CAS 2: MISE EN COUPLE
  // --------------------------------------
  
  
  if (matri_status[start]==2){
    
    // -------------------------------------------------
    // 1: patrimoine referent: nécessairement homme
    
    if (referent_last_year){
      
      // Homme était en couple avec qqun d'autre: divorce puis remariage
      if ((matri_status[0]==2) & (spouse_id[1] != spouse_id[0])){
        inherit += patrimoine[0]/2;
      } else{
        // Homme garde même compagne: patrimoine identique
        inherit += patrimoine[0];
      }
      
    }
    // if not referent last year: no personal wealth to transmit, i.e. nothing to do
    
    
    // ----------------------------
    // 2: patrimoine conjoint
    // NB: lead volontaire qui fait que la situation passee du conjoint est indexée 1
    
    if (referent_last_year){ //i.e. referent last year
      
      if (matri_conjoint[1] == 1){
        // La femme etait celibataire mais pas son propre referent
        inherit += 0;
      } else if (matri_conjoint[1] == 2){
        if (spouse_id[1] != spouse_id[0]){
          // Femmme en couple avant: divorce puis remariage direct
          inherit += patrimoine_conjoint[1]/2;
        }
      } else{
        // Femme arrive avec son patrimoine (éventuellement 0)
        if (!NumericVector::is_na(patrimoine_conjoint[1])) inherit += patrimoine_conjoint[1];
      }
    } else{
      
      //std_niveau += 0;
      //std_patri += 0;
      if (!NumericVector::is_na(patrimoine_conjoint[1])) inherit +=patrimoine_conjoint[1]/2;
      
    }
    
    
  }
  
  
  // --------------------------------------
  // CAS 3: DECES DU CONJOINT
  // --------------------------------------
  
  if (matri_status[start] == 3){ //Cas du deces du conjoint
    
    if (sexe[start]==1){ // Homme: nécessairement son propre référent l'année d'avant
      inherit += patrimoine[0];
    } else{ // Femme: n'était pas son propre référent l'année d'avant (herite patrimoine conjoint et son patrimoine eventuel)
      inherit += (patrimoine[0] + patrimoine_conjoint[start]);
    }
    
  }
  
  // --------------------------------------
  //  CAS 4: Divorce
  // --------------------------------------
  
  if (matri_status[start] == 4){
    // if (sexe[1] == 2 | age[1] <= ageliquidation[1]){
    //   // Si la personne n'était pas référente
    //   if ((spouse_id[1] == spouse_id[0]) | spouse_id[0] == id[0]){
    //     // Si son référent faisait partie du couple
    //     inherit += patrimoine[0]/2;
    //     standard_niveau[1] = standard_niveau[0];
    //     standard_patrimoine[1] = standard_patrimoine[0];
    //   }
    // } else{
    if (sexe[start]==1){
      // Cas d'un homme referent de son ancien menage
      inherit += patrimoine[0]/2;
    } else{
      // Cas d'une femme: aupravant pas référente
      inherit += patrimoine_conjoint[0]/2;
    }
  }
  
  
  List x ;
  x["Kprev"] = patrimoine[0];
  x["patrimoine"] = inherit ;
  //Tests only: x['referent_last_year'] = referent_last_year;
  //x["referent_last_year"] = referent_last_year;
  
  return x;
}




