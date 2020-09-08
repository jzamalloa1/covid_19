import dash
import dash_core_components as dcc
import dash_html_components as html
import plotly.express as px
import plotly.graph_objects as go
import pandas as pd
import numpy as np
import keplergl, urllib, sys, importlib
import geopandas as gpd
import dash_bootstrap_components as dbc


# THEMES
app = dash.Dash(
    external_stylesheets=[dbc.themes.SLATE]
)

# PROCESS DATA
sys.path.append('/mnt/c/Users/JOSE/Documents/PROJECT/COVID19/MODELS')
import google_feat_gen
importlib.reload(google_feat_gen)
from google_feat_gen import *
# from lstm_covid_v2 import rnn_covid
from covid_plotly import *

google_epi = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/epidemiology.csv")
google_index = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/index.csv")
google_demo = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/demographics.csv")
google_geo = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/geography.csv")

countries = ["Argentina", "Brazil", "Peru", "Colombia", "Mexico", "South Africa", "Chile",
             "Italy", "Spain", "Australia", "Canada", "Netherlands", "United Kingdom", "India"]

keys = google_level_keys(google_index, countries, 1)
main_epi = google_epi_level_2(keys, google_epi, need_deaths=False)
main_epi = prep_incidence(main_epi, google_demo, google_index, assign_names=True)


## APP STRUCTURE
first_tab = dbc.Container(
    [
        dbc.Row(dbc.Col(html.Div("Placeholder title"), width=6)),

        dbc.Row([
            dbc.Col(
                dcc.Dropdown(id="plot_1_select",
                    options=[{"label":i, "value":i} for i in set(main_epi.country_name)],
                    multi=True,
                    placeholder="Select countries",
                    value="Brazil"
                ),
                width=3
            )
        ], no_gutters = False, justify="end"),
        html.Br(),
        dbc.Row(
            [
                dbc.Col(
                    dcc.Graph(id="plot_1"),
                    width=12
                )
            ]
        )
    ],
    fluid=True
)

second_tab = dbc.Container(
    [
        dbc.Row(dbc.Col(html.Div("Placeholder title"), width=6)),

        dbc.Row([
            dbc.Col(
                dcc.Dropdown(id="plot_23_select",
                    options=[{"label":i, "value":i} for i in set(main_epi.country_name)],
                    multi=False,
                    placeholder="Select country of interest",
                    value="Italy"
                ),
                width=3
            )
        ], no_gutters = False, justify="end"),
        html.Br(),
        dbc.Row(
            [
                dbc.Col(
                    dcc.Graph(id="plot_2"),
                    width=8
                ),
                dbc.Col(
                    dcc.Graph(id="plot_3"),
                    width=4
                )
            ],
            no_gutters=True
        )
    ],
    fluid=True
)



## STRUCTURE FUNCTIONS
@app.callback(dash.dependencies.Output("plot_1", "figure"),
              [dash.dependencies.Input("plot_1_select", "value")])

def plot_1_figure(plot_1_country_input):

    if  plot_1_country_input is None or len(plot_1_country_input)==0:
        temp_table = main_epi
    else:
        temp_table = main_epi.query("country_name in @plot_1_country_input")

    return covid_plot_1(temp_table)


@app.callback(dash.dependencies.Output("plot_2", "figure"),
              [dash.dependencies.Input("plot_23_select", "value")])

def plot_2_figure(plot_2_country_input):

    if  plot_2_country_input is None or len(plot_2_country_input)==0:
        temp_table = main_epi.query("country_name=='Italy'")
    else:
        temp_table = main_epi.query("country_name==@plot_2_country_input")

    return covid_plot_2(temp_table)


@app.callback(dash.dependencies.Output("plot_3", "figure"),
              [dash.dependencies.Input("plot_23_select", "value")])

def plot_3_figure(plot_3_country_input):

    if  plot_3_country_input is None or len(plot_3_country_input)==0:
        temp_table = main_epi.query("country_name=='Italy'")
    else:
        temp_table = main_epi.query("country_name==@plot_3_country_input")

    return covid_plot_3(temp_table)

## MASTER LAYOUT
tabs = dbc.Tabs(
    [
        dbc.Tab(first_tab, label="Main map"),
        dbc.Tab(second_tab, label="Incidence tracking")
    ]
)

app.layout = dbc.Container(
    [
        html.H1("COVID-19 PANEL"),
        html.Hr(),
        tabs
    ],
    fluid=True
)

## RUN
if __name__ == "__main__":
    app.run_server(debug=True)
