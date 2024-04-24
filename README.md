# Russian Regestries Data Processing in R

The repo contains workflow of processing and checking russian cancer regestries data. For processing simple operations of filtering and renaming are done. For checking IACR CHeck Tool implementation is done. More detailed description of it is presented.  

The structure of worklow is the following:

**1_read_and_name_the_data.R**
The script read raw data and translates variable names and values. In addition some simple operation with variables are done: split of ICD-10 codes, determination of stage, calculation of age, filter the only malignant behaviour and etc.    

**2_converse_the_data.R**
The script checks for ICD-O-2 errors in coding of diagnosis and converse data. The flow of conversion: if morph variable is NA then conversion form ICD-10 to ICD-O-2, else from ICD-O-2 to ICD-O-3 and from ICD-O-3 back to ICD-10. 

**3_fix_the_data.R**
The script emulate IARC Check Tool and fixes the errors in data, according to IARC recommendations. 

## IACR Check Tool implementation's description

The functions (functions_for_iacr_mp and functions_for_check) helps with Data Preprocessing by imitating the behavior of IACR Check for data from cancer regestries. The current implementation is specified to preproccesed data from Russian cancer registries. 

Requirements for a proper functioning:
- ICD-O-3 code should be separated into columns 
- "topo_Cxxx", "topo_Cxxx", "morph", "behav_icdo3", "gr_icdo3"
- Columns "morph", "behav_icdo3", "gr_icdo3" should have type double

### The functions implemented to a files:
1. **add_histology_families**
   Adding  histology family based on 'morph' column.
   
2. **check_site_histology**
   Check for site-histology combinations, using 'hist_family' and 'topo' variables
   
3. **check_dates_and_age**
   Check for avaliability of dates and no contradiction between them, using 'diag_date' and 'dob_date' variables
    
4. **check_sex_site**
   Check sex-site combinations based on 'sex' and 'topo' variables

5. **check_age_site_histology**
   Check age-site-histology combinations based on 'morph', 'topo' and 'age' variables

6. **check_sex_histology**
   Check sex-histology combinations, using 'hist_family' and 'sex' variables

7. **check_behaviour_site**
   Check behaviour-site combinations, using 'topo' and 'behav_icdo3' variables

8. **check_behaviour_histology**
   Check behaviour-histology combinations, using 'data_for_behaviour_histology_control' file

9. **check_grade_histology**
   Check grade-histology combinations, using 'morph' and 'gr_icdo3' variables

10. **check_basis_histology**
   Check basis-histology combinations, using 'basis' and 'morph' variables

--------------
The original tool is done by IACR: http://www.iacr.com.fr/index.php?option=com_content&view=category&layout=blog&id=68&Itemid=445
