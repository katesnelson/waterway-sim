# waterway-sim
Inland waterway navigation simulation

This repo holds the code (Netlogo) for an agent-based model of inland waterway tow traffic in the context of extreme weather events conceptualized in 

NELSON, K., CAMP, J., & PHILIP, C. Paper 123-Navigable Inland Waterway Transportation Modeling: A Conceptual Framework and Modeling Approach for Consideration of Climate Change Induced Extreme Weather Events. 

and used in simulations reported in  

Nelson, K. S., Camp, J. V., Philip, C. E., & Abkowitz, M. D. (2017). Agent-Based Model of Navigable Inland Waterway Tow Operation Procedures. Transportation Research Record: Journal of the Transportation Research Board, (2611), 11-18.


The model allows for some customization by location and includes a set of modifiable parameters that are revelvant to tow operation decisions made during extreme weather events and that are influenced by official Waterway Action Plan guidance documents.

To run model:
Install Netlogo 3D version 3.2 or 3.3
Download the time, xw, and csv extensions into the Netlogo program extension folder
Open the model and input a filename (to save model output to) and operating parameters for the model run (I use 8 digit date and one alpha numeric character, processing scripts in R are based on this naming convention)
Go to the River Setup Tab set location of locks and dams
Go back to the Interface tab and click go


Also included in the repo are an updated version of the model and R scripts used in processing simulation data in a validation study.

