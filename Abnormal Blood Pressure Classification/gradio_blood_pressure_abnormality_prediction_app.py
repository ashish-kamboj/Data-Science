#*******************************************************************************************************************************************************
## Loading Libraries
#*******************************************************************************************************************************************************

import gradio as gr
import pandas as pd
import numpy as np
import warnings
import os

warnings.filterwarnings("ignore")

#*******************************************************************************************************************************************************
## Loading Training Data
#*******************************************************************************************************************************************************

current_dir = os.path.dirname(os.path.realpath(__file__))
df = pd.read_csv(os.path.join(current_dir, 'Training Data - Classification of Patients with Abnormal Blood Pressure ....csv'))


#*******************************************************************************************************************************************************
## Model Building - Using PyCaret
#*******************************************************************************************************************************************************

from pycaret.classification import * 
clf = setup(df, target='Blood_Pressure_Abnormality', session_id=42, fold=5, train_size=0.9,
            data_split_stratify=True, normalize=True, normalize_method='minmax',
            imputation_type='iterative', ignore_features=['Patient_Number'], remove_outliers=True)


#*******************************************************************************************************************************************************
## Model Comparison
#*******************************************************************************************************************************************************

# Comparing baseline classification models (then try out the top ones)
choose_best_model = compare_models(sort='Accuracy', fold=5)
compare_model_results = pull()


#*******************************************************************************************************************************************************
## ## Creating Gardio aplication
#*******************************************************************************************************************************************************

# Creating a predict function to be passed into gradio UI
def predict_abnormality(model, Genetic_Pedigree_Coefficient, Level_of_Hemoglobin, salt_content_in_the_diet, alcohol_consumption_per_day, Physical_activity, BMI, Age, Chronic_kidney_disease, Adrenal_and_thyroid_disorders, Smoking, Level_of_Stress, Sex, Pregnancy):
  
    df = pd.DataFrame.from_dict({'Level_of_Hemoglobin': [Level_of_Hemoglobin], 'Genetic_Pedigree_Coefficient': [Genetic_Pedigree_Coefficient], 'Age': [Age], 
                                 'BMI':[BMI], 'Sex':[Sex],'Pregnancy': [Pregnancy], 'Smoking': [Smoking], 'Physical_activity': [Physical_activity], 
                                 'salt_content_in_the_diet':[salt_content_in_the_diet],'alcohol_consumption_per_day':[alcohol_consumption_per_day],
                                 'Level_of_Stress':[Level_of_Stress], 'Chronic_kidney_disease':[Chronic_kidney_disease], 'Adrenal_and_thyroid_disorders':[Adrenal_and_thyroid_disorders]})
    
    model_index = list(compare_model_results['Model']).index(model)
    model = choose_best_model[model_index]
    pred = predict_model(model, df, raw_score=True)
    
    return {'1': pred['Score_1'][0].astype('float64'), 
            '0': pred['Score_0'][0].astype('float64' )}


#*******************************************************************************************************************************************************
## Input Components

model = gr.inputs.Dropdown(list(compare_model_results['Model']),label="Model")

Chronic_kidney_disease = gr.inputs.Dropdown(choices=["1", "0"],label = 'Chronic kidney Disease')
Adrenal_and_thyroid_disorders = gr.inputs.Dropdown(choices=["1", "0"],label = 'Adrenal and thyroid disorders')
Sex = gr.inputs.Dropdown(choices=["0", "1"],label = 'Sex')
Pregnancy = gr.inputs.Dropdown(choices=["0", "1"],label = 'Pregnancy')
Smoking = gr.inputs.Dropdown(choices=["0", "1"],label = 'Smoking')
Level_of_Stress = gr.inputs.Dropdown(choices=["1", "2", "3"],label = 'Level of Stress')

Level_of_Hemoglobin = gr.inputs.Slider(minimum=0, maximum=20, step=0.1, default=round(df['Level_of_Hemoglobin'].mean(),1), label = 'Level of Hemoglobin')
Genetic_Pedigree_Coefficient = gr.inputs.Slider(minimum=0, maximum=1, step=0.01, default=round(df['Genetic_Pedigree_Coefficient'].mean(),2), label = 'Genetic Pedigree Coefficient')
salt_content_in_the_diet = gr.inputs.Slider(minimum=10, maximum=50000, step=1, default=int(df['salt_content_in_the_diet'].mean()), label = 'Salt content in the diet')
alcohol_consumption_per_day = gr.inputs.Slider(minimum=0, maximum=500, step=1, default=int(df['alcohol_consumption_per_day'].mean()), label = 'Alcohol consumption per day')
Age = gr.inputs.Slider(minimum=1, maximum=100, step=1, default=int(df['Age'].mean()), label = 'Age')
BMI = gr.inputs.Slider(minimum=0, maximum=100, step=1, default=int(df['BMI'].mean()), label = 'BMI')
Physical_activity = gr.inputs.Slider(minimum=100, maximum=50000, step=10, default=int(df['Physical_activity'].mean()), label = 'Physical activity')

## Creating an Interface
iface = gr.Interface(fn = predict_abnormality,
                     inputs = [model, Genetic_Pedigree_Coefficient, Level_of_Hemoglobin, salt_content_in_the_diet, alcohol_consumption_per_day, Physical_activity, BMI, Age, Chronic_kidney_disease, Adrenal_and_thyroid_disorders, Smoking, Level_of_Stress, Sex, Pregnancy], 
                     outputs = "label",
                     live = True,
                     title = "Classification of Patients with Abnormal Blood Pressure",
                     description = "Experiment tweaking the below mentioned features to check which factor affects the most in predicting the abnormality",
                     interpretation = "default") 


#*******************************************************************************************************************************************************

## Launching application
if __name__ == "__main__":
    iface.launch(inbrowser=True)