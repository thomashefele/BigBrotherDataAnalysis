# Big Brother/Big Sister Mentor-Mentee Research Project
A computer program for performing data analysis on the effects of racial and ethnic match on mentor-mentee relationships in the Big Brother/Big Sister organization. Done in support of Jennifer Koide's PhD research thesis "Effects of Ethnic–Racial Identity on Strength of Mentorship Relationships."

This program mines data from Big Brother/Big Sister surveys to compare the survey scores between the baseline and a target interval of interest. From this data mining, the program also compares the racial and ethnic identities of the big sibling to the little sibling to determine if there is a racial/ethnic match or not.

## Use:

- Order a SOR (Strength of Relationship) data sheet first by "Match ID" alphabetically and then by Completion Date (from earliest date to latest)
- Order a Match History Detail date sheet by "Match ID" alphabetically.
- Run the program in a R notebook. You will be prompted several separate times:
  - First, to enter the pathway of the SOR file:
  
  <img width="1175" alt="BBBS1" src="https://user-images.githubusercontent.com/116929892/223919531-6f992b32-2be7-44fe-aad1-777e85b030e3.png">

  - Second, to enter the pathway of the Match History file:
  
  <img width="1174" alt="BBBS2" src="https://user-images.githubusercontent.com/116929892/223919503-89060560-fc8f-438b-b26c-75b0cf7993e5.png">
  
  - Lastly, to enter the time interval of interest for comparison of survey scores to the baseline (+/- a user-stated deviation):
  
  <img width="1175" alt="BBBS3" src="https://user-images.githubusercontent.com/116929892/223919473-6da0430b-981d-4b1c-aa85-e84baf110337.png">
  
Afterwards, your work is done! The program will now sift through the data to produce CSV files (one for the big, one for the little) of all cases with completed surveys at both the baseline and the target time interval, along with whether or not a racial or ethnic match occured in each case.
