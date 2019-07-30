#' Anatomic and Therapeutic Chemical classification system
#' 
#' Anatomic and Therapeutic Chemical (ATC) classification system is an international nomenclature for drugs 
#'
#' @format A data frame with 4623 rows and 10 variables:
#' \describe{
#'   \item{atc5}{Code of 7 elements defining a drug. Compare to level 4, the fifth level of the code indicates the chemical substance and consists of two digits.}
#'   \item{atc4}{Character of size 5.The fourth level of the code indicates the chemical/therapeutic/pharmacological subgroup and consists of one letter.}
#'   \item{atc3}{Character of size 4.The third level of the code indicates the therapeutic/pharmacological subgroup and consists of one letter.}
#'   \item{atc2}{Character of size 3. The second level of the code indicates the therapeutic subgroup and consists of two digits.}
#'   \item{atc1}{The first level of the code indicates the anatomical main group and consists of one letter}
#'   \item{label5}{Label for act5: chemical substance}
#'   \item{label4}{Label for act4: chemical/therapeutic/pharmacological subgroup}
#'   \item{label3}{Label for act3: therapeutic/pharmacological subgroup}
#'   \item{label2}{Label for act2: therapeutic subgroup}
#'   \item{label1}{Label for act1: the anatomical main group}
#' }
"ATC"


#'  Drug Interaction Knowledge Base data
#'  
#'  Curated version of the Drug Interaction Knowledge Base (DIKB). The modalities of the various variables depend on the  data source (ie. KEGG, NDFRT, ANSM). See reference for more details
#' 
#'@format   Data frame of 26 variables by 91 553 rows.
#'  \describe{
#'    \item{drug.pairs}{Unique drug bank ID pair (object*precipitant)}
#'    \item{Kegg}{Kegg precaution evaluation of a drug bank ID pair (between 1 and 3)}
#'    \item{FrenchDB}{FrenchDB precaution evaluation of a drug bank ID pair (between 1 and 3)}
#'    \item{NDFRT}{NDFRT precaution evaluation of a drug bank ID pair (between 1 and 3)}
#'    \item{OSCAR}{OSCAR precaution evaluation of a drug bank ID pair (between 1 and 3)}
#'    \item{WorldVista}{WorldVista precaution evaluation of a drug bank ID pair (between 1 and 3)}
#'    \item{mean}{Mean of the 5 precaution evaluations of the 5 different sources (between 1 and 3)}
#'    \item{max}{Maximum of the 5 precaution evaluations of the 5 different sources (between 1 and 3)}
#'    \item{diff.mean.max}{Difference between mean and max of the 5 precaution evaluations of the 5 different sources (between 0 and 1.5)}
#'    \item{score.max}{Final score of precaution for a pair of drug bank ID (calcul : if(mean>4/3){max-1/10*diff.mean.max)} else{max-diff.mean.max}}
#'    \item{drug.id.object}{Drug bank ID of the first (object) interacting drug}
#'    \item{drug.id.precipitant}{Drug bank ID of the second (precipitant) interacting drug}
#'    \item{atc.object}{ATC code the first (object) interacting drug}
#'    \item{atc.precipitant}{ATC code the second (precipitant) interacting drug}
#'    \item{label1.atc.object}{Anatomical main group of the object ATC code}
#'    \item{label1.atc.object}{Anatomical main group of the object ATC code}
#'    \item{object}{Full name of the the first interacting drug}
#'    \item{precipitant}{Full name of the the second interacting drug}
#'    \item{label2.atc.object}{therapeutic subgroup of the object ATC code}
#'    \item{label2.atc.precipitant}{therapeutic subgroup of the object ATC code}
#'    \item{label3.atc.object}{therapeutic/pharmacological subgroup of the object ATC code}
#'    \item{label3.atc.precipitant}{therapeutic/pharmacological subgroup of the object ATC code}
#'    \item{label4.atc.object}{chemical/therapeutic/pharmacological subgroup of the object ATC code}
#'    \item{label4.atc.precipitant}{chemical/therapeutic/pharmacological subgroup of the object ATC code}
#'    \item{label5.atc.object}{chemical substance of the object ATC code}
#'    \item{label5.atc.precipitant}{chemical substance of the object ATC code}
#'  }
#' @references Ayvaz et al (2015) Toward complete dataset of drug-drug interaction information from publicly available sources. Journal of Biomedical Informatics, 55: 206-217.
#' 
#' @references  Richard D. Boyces group at University of Pittsburgh. See <https://dbmi-icode-01.dbmi.pitt.edu/dikb-evidence/pddi-sets/>
#' @author Y. Rivault, N. Le Meur and S. Tessier
#' 
"DIKB"


#'  Drug Indication Database
#'  
#'  Curated version of the Drug Indication Database (DID) downloaded from supplementary material of Sharp ME (2017).
#'  
#' @format Data frame of 66 variables by 464 056 rows.
#'  \describe{ 
#'  Drugs and their phenotype indications (first two variables) are coded according to Concept Unique Identifier (CUI) codification from the Unified Medical Language System (UMLS). The other variables define their characteristics.
#'  }
#' 
#' @references harp ME (2017). Toward a comprehensive drug ontology: extraction of drug-indication relations from diverse information sources. Journal of Biomedical Semantics,8:2.
#' @references Sharp ME (2017) supplementary material: <https://figshare.com/articles/Additional_file_1_of_Toward_a_comprehensive_drug_ontology_extraction_of_drug-indication_relations_from_diverse_information_sources/4535021>
#' 
#' @author Y. Rivault and N. Le Meur
"DID"

#' Disease set
#' 
#' Example  of patients diagnosis
#' 
#' @format dataframe  1997 rows by 4 variables that contains patients Id and prescribed drugs, codified according to the ICD10 and mapped to cui and NDFRT.
#' @author Y. Rivault
"disease_set"

#' Drug set
#' 
#' Example of prescribed drug to patients
#' 
#' @format dataframe  13421 rows by 4 variables that contains patients Id and prescribed drugs, codified according to the ATC and mapped to cui and NDFRT.
#' @author Y. Rivault
"drug_set"
