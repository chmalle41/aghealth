# aghealth

Integrated model for assessment of diet, air pollution and climate impacts of food consumption and production

The aghealth model is a model developed in the R statistical software for integrated assessent of the impacts of food consumption and production on climate, air pollution and human health through dietary, obesity and malnutrition. 

Aghealth quantifies emissions of greenhouse gases and air pollutants from livestock and crop production, and the health impacts from diet, malnutrition and obesity at national scale. It is a demand-driven model, and takes as input the population, and average calorie consumption, broken down by different food types. The balance of imports, exports and domestic consumption of different foods then determines the domestic production of livestock (cows, sheep/goats, pigs and chickens) and crop production. The agricultural systems that meet the demand for different foods are then modelled to quantify the greenhouse gas and air pollutant emissions. The overall modelling framework is shown below. The model contains 5 main modules, i) Food Demand, ii) Livestock, iii) Crop Production, iv) Pasture Lands, and v) Human Health impact assessment. 

![image](https://user-images.githubusercontent.com/83307652/116305519-cca3fa00-a79b-11eb-9bd1-7e69271e68a6.png)

The model is programmed in the R Statistical Software (v.4.0.3) and requires the plyr, numbers and mc2d packages to run. 

The model has been developed with funding from the Stockholm Environment Institute Integrated Climate and Development Planning initiative. It has been developed by Chris Malley, with support from Kevin Hicks, Johan Kuylenstierna, Eleni Michalopoulou, Jessica Slater, Charlie Heaps, Jason Veysey, Silvia Ulloa, Daven Henze, Omar Nawaz, Susan Anenberg, Drew Shindell, Brian Mantlana and Timothy Robinson. 

If you have queries or questions, please contact Chris Malley, Stockholm Environment Institute, University of York (chris.malley@york.ac.uk). 

