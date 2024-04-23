#include <Rcpp.h>
using namespace Rcpp;

//' Determine household head for individual level data (C++ version)
//' 
//' \code{Rcpp} function to determine household referent from
//'  individual data. This is a necessary step to transform individual
//'  level data into household data. Vector should all be numeric formatted
//'  and the same length (internally this function uses a for loop).
//'  
//' @seealso \link{assign_referent} ; \link{assign_referent_R}
//'  
//' @details The following rules are applied:
//' \itemize{
//' \item{\emph{Immigrants}: Arrive without referent}
//' \item{\emph{Couples}: Male is assumed to be the head}
//' \item{\emph{Husband/Spouse death}: Individual is his/her own head}
//' \item{\emph{Divorce}: Individual is his/her own head}
//' \item{\emph{Potentially under father/mother authority}:
//'  If education is accomplished or young adult already earns income,
//'  individual is his/own head.
//' Father is assumed to be the household head. If father is dead,
//'  mother is the head. If she is dead, individual is his/her own head}
//' }
//' 
//' @param id Individual identifier
//' @param annee Observation/Simulation year
//' @param conjoint Husband/spouse identifier. Single people
//'  will be imputed a 0 value
//' @param sexe Sex. Males are coded 1 and females 2.
//' @param matri Matrimonious or family status. \emph{1}: leave parents ;
//'  \emph{2}: couple ; \emph{3}: husband/spouse death ; \emph{4}: divorce
//' @param referent,referent2 Household heads vector that will be imputed.
//'  Recommended to provide a vector of missing values.
//' @param findet Age where inidivudal stops education
//' @param age Current age of the individual
//' @param salaire Individual income
//' @param pere Father identifier
//' @param ageMaxPere Father death age
//' @param mere Mother identifier
//' @param ageMaxMere Mother death age
//' @param neFrance Origin of the individual. 1 for individual born
//'  in France ; 0 for immigrants
//'  
//' @return Dataframe storing individual identifier (\code{id}),
//'  year (\code{annee}), matrimonious status (\code{matri}),
//'  husband/spouse identifier (\code{conjoint}) and referent
//'  identifier (\code{referent})
//' @export
//[[Rcpp::export]]
Rcpp::DataFrame assign_referent_cpp(NumericVector id,
                                    NumericVector annee,
                                    NumericVector conjoint,
                                    NumericVector sexe,
                                    NumericVector matri,
                                    NumericVector referent,
                                    NumericVector referent2,
                                    NumericVector findet,
                                    NumericVector age,
                                    NumericVector salaire,
                                    NumericVector pere,
                                    NumericVector ageMaxPere,
                                    NumericVector mere,
                                    NumericVector ageMaxMere,
                                    NumericVector neFrance){
  
  // DATAFRAME THAT WILL BE RETURNED  
  Rcpp::DataFrame x ;
  
  // MAIN for LOOP  
  for (int i=0;i<id.size();i++){
    
    // =================================
    //              CASE 1
    //            IMMIGRANTS
    // =================================
    
    // THEY ARRIVE WITHOUT HOUSEHOLD HEAD
    
    if (NumericVector::is_na(matri[i])){
      // MISSING VALUES
      conjoint[i] = 0;
      matri[i] = 1;
    }
    
    
    // ====================================
    //                CASE 2
    //          MATRI==2 (COUPLES)
    // ====================================
    
    // MALE ARE ASSUMED TO BE THE REFERENT
    
    if (matri[i]==2){
      if (sexe[i]==1){
        referent[i] = id[i];
      }else{
        referent[i] = conjoint[i];
      }
    }
    
    
    // ================================
    //              CASE 3:
    //      MATRI==3 (MORT CONJOINT)
    //         MATRI==4 (DIVORCE)
    // ================================
    
    // INDIVIDUAL BECOMES HIS/HER OWN REFERENT
    
    if ((matri[i]==3) | (matri[i]==4)) referent[i]=id[i];
    
    
    // =============================
    //            CASE 4
    //           MATRI==1
    // =============================
    
    if (matri[i] < 2){
      // Remaining cases: 0 or 1
      
      if (age[i] > findet[i] || salaire[i] != 0){
        // SINGLE INDIVIDUAL THAT HAS STOPPED STUDYING
        // OR EARN INCOME
        referent[i]=id[i];
      } else{
        // INDIVIDU DEPEND POTENTIELLEMENT DE SES PARENTS
        
        if (pere[i] != 0){
          // PERE EST CONNU
          
          if (ageMaxPere[i] > age[i]){
            // PERE TOUJOURS VIVANT
            
            // Referent 1: PERE
            referent[i] = pere[i];

            // Referent 2: MERE (SI VIVANTE)
            if (ageMaxMere[i] > age[i]){
              referent2[i] = mere[i];
            }
            
          }else{
            
            // PERE EST MORT
            if (mere[i] != 0){
              // DANS CE CAS, SI LA MERE EST CONNUE
              if (ageMaxMere[i] > age[i]){
                // MERE VIVANTE: REFERENTE
                referent[i] = mere[i];
              } else{
                // MERE MORTE: INDIVIDU EST SON PROPRE REFERENT
                referent[i] = id[i];
              }
            }
          }
        } else if (mere[i] !=0){
          // PERE INCONNU MAIS MERE CONNUE
          if (ageMaxMere[i] > age[i]){
            // MERE VIVANTE: REFERENTE
            referent[i] = mere[i];
          } else{
            // MERE MORTE: INDIVIDU EST SON PROPRE REFERENT
            referent[i] = id[i];
          }
        } else{
          // DEUX PARENTS INCONNU
          if (neFrance[i]==0){
            referent[i] = 0;
          } else{
            referent[i] = id[i];
          }
        }
        
      }
      
      
      
    }
    
    
  } // END FOR LOOP
  
  // INFORMATION RETURNED  
  x["Id"] = id;
  x["annee"] = annee;
  x["matri"] = matri;
  x["conjoint"] = conjoint;
  x["referent"] = referent;
  
  return(x);
}

