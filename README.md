# IACR Check Tool implementation in R (iarc_check_sim)

The functions helps with Data Preprocessing by imitating the behavior of IACR Check for data from cancer regestries. The current implementation is specified to preproccesed data from Russian cancer registries. 

Requirements for a proper functioning:
- ICD-O-3 code should be separated into columns 
- "topo_Cxxx", "topo_Cxxx", "morph", "behav_icdo3", "gr_icdo3"
- Columns "morph", "behav_icdo3", "gr_icdo3" should have type double

## The functions implemented to a file:
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
