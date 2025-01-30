# Hotel Booking Cancellation Prediction

## Project Overview
This project focuses on predicting hotel booking cancellations using machine learning techniques. The dataset is sourced from Kaggle and includes various features such as booking details, customer information, and hotel-specific factors that influence cancellation behavior.

The key components of the project include:
- **Data Preprocessing**: Cleaning and preparing the dataset by handling missing values, removing duplicates, and transforming variables using R programming.
- **Feature Engineering**: Creating new meaningful features and applying encoding techniques to categorical variables using R programming.
- **Model Development**: Implementing Decision Tree and XGBoost models for classification in Jupyter Notebook.
- **Evaluation**: Assessing model performance using accuracy, precision, recall, and F1-score in Jupyter Notebook.

## Dataset
The dataset used for this project is sourced from Kaggle:
- [Hotel Booking Demand Dataset](https://www.kaggle.com/jessemostipak/hotel-booking-demand)

## Tools & Technologies Used
- **Programming Languages**: R, Python
- **Libraries**:
  - R: `dplyr`, `caret`, `ggplot2`, `lubridate`, `corrplot`, `skimr`
  - Python: `scikit-learn`, `xgboost`, `pandas`, `matplotlib`, `seaborn`
- **Jupyter Notebook** for model development and evaluation

## Setup Instructions
### 1. Download the Repository
- Click the "Download" button on the repository page.
- Select **Download ZIP**.
- Extract the downloaded ZIP file to your desired location.

### 2. Install Dependencies
#### R Environment
Ensure the necessary R packages are installed:
```r
install.packages(c("dplyr", "caret", "ggplot2", "lubridate", "corrplot", "skimr"))
```
#### Python Environment
Install required libraries using:
```bash
pip install scikit-learn xgboost pandas matplotlib seaborn jupyterlab
```

### 3. Running the Project
1. Open your terminal.
2. Launch Jupyter Notebook:
   ```bash
   jupyter notebook
   ```
3. In the Jupyter interface, navigate to the project directory.
4. Open and run the notebook files (`Decision Tree and XGboost_6851162.ipynb`).

### 4. Troubleshooting
If you encounter any issues:
- Verify that all required dependencies are installed.
- Ensure you are in the correct directory when launching Jupyter Notebook.
- Modify file paths in the notebook if necessary.

## Contributions
| Contributor  | Contribution |
|-------------|-------------|
| Anisha Rajesh | Data preprocessing, Decision Tree implementation, XGBoost model |



