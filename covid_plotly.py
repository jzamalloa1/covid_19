import plotly.express as px
import plotly.graph_objects as go
import pandas as pd
import numpy as np
from plotly.subplots import make_subplots

def covid_plot_1(epi_df):
    # Plots annualized incidence vs cumulative incidence

    main_plot = (epi_df
             .groupby(["key"]).date.max()
             .reset_index(name="max_date")
             .merge(epi_df, on="key")
             .query("max_date==date")
             .assign(above_annual = lambda d: d.annual_inc>0.015)
#              .query("country_name=='Peru'")
            )

    sizeref   = 2.*max(main_plot['population'])/(35**2)
    countries = list(set(main_plot.country_name))

    # Create figure
    fig = go.Figure()

    for i in countries:
        i_table = main_plot.query("country_name==@i", engine="python")
        fig.add_trace(go.Scatter(x=i_table.cum_inc, y=i_table.annual_inc, name=i, 
                                text=i_table.subregion1_name,
                                hovertemplate="<b>%{text}</b><br><br>" +
                                            "Annualized Inc. : %{y:.1%}<br>" +
                                            "Cumulative Inc. : %{x:.1%}<br>" +
                                            "Population: %{marker.size:,}",
                            marker=dict(
                                size=i_table.population
                            )))

    fig.add_shape(
            # Line Vertical
            dict(
                type="line",
                x0=0.04,
                y0=0,
                x1=0.04,
                y1=main_plot.annual_inc.max(),
                line=dict(
                    color="Crimson",
                    width=1,
                    dash="dashdot"
                )
    ))
    fig.add_shape(
            # Line Horizontal
                type="line",
                x0=0,
                y0=0.015,
                x1=main_plot.cum_inc.max(),
                y1=0.015,
                line=dict(
                    color="Red",
                    width=1,
                    dash="dashdot",
                ),
        )
        
    # Tune marker appearance and layout
    fig.update_traces(mode='markers', marker=dict(sizemode='area',
                                                sizeref=sizeref, line_width=2))
    fig.update_layout(template="plotly_dark",
                    title='Regional Incidence' +
                            "<br><sub>Annualized vs cumulative incidence across all provinces</sub>",
                    xaxis=dict(
                        title='Cummulative Incidence',
    #                     gridcolor='grey',
    #                     type='log',
                        gridwidth=2,
                        tickformat=".0%",
                        ),
                    yaxis=dict(
                        title='Annualized Incidence',
    #                     gridcolor='white',
                        gridwidth=2,
                        tickformat=".0%",
                        ),
                    height=600,
                    # hovermode="x"
                    )

    return fig

def covid_plot_2(epi_df):
    # Per region stacked plot of standardized annualized incidence. One country at a time allowed
    
    regions = list(epi_df
           .loc[:,["subregion1_name", "std_last"]]
           .drop_duplicates()
           .sort_values(["std_last"], ascending=False)
                .subregion1_name
          )

    fig = make_subplots(rows=len(regions), cols=1,
                   shared_xaxes=True, shared_yaxes=True,
                   subplot_titles=regions,
                   vertical_spacing=0.001)

    for i in range(len(regions)):
        region = regions[i]
        i_table = (epi_df
                .query("subregion1_name==@region", engine="python")
                )
        population = list(i_table.population)[0]
        
        fig.append_trace(
            go.Bar(
                x=i_table.date, y=i_table.std_inc, 
                marker=dict(color=i_table.std_inc, coloraxis="coloraxis"),
                text=i_table.annual_inc,
                hovertemplate="<b>"+region+"</b><br>" +
                                "Annualized Inc. : %{text:.2%}<br>" +
                                "Date : %{x|%m-%d-%y}<br>" +
                                "Population: {:,}".format(population),
                name=""
            ),
            row=i+1, col=1
        )

    fig.for_each_annotation(lambda a: a.update(x=0.03))
    fig.for_each_annotation(lambda a: a.update(xanchor="left"))
    fig.for_each_annotation(lambda a: a.update(yanchor="top"))
    fig.for_each_annotation(lambda a: a.font.update(size=13))
    fig.layout["coloraxis"]["colorbar"]["title"]["text"]="Standardized<br>Incidence"
    fig.update_yaxes(visible=False, showticklabels=False)
        
    fig.update_layout(height=np.max([len(regions)*60, 600]), template="plotly_dark",showlegend=False,
                    title='Regional Standardized Incidence' +
                            "<br><sub>Annualized incidence standardized between 0-1 per region</sub>")

    return fig

def covid_plot_3(epi_df):
    # Per region stacked plot of annualized incidence (latest time point). One country at a time allowed
    
    epi_df = (epi_df
    .loc[:,["subregion1_name", "annual_inc_last", "cum_inc_last", "std_last", "population"]]
    .drop_duplicates()
    .sort_values(["std_last"], ascending=False)
    )

    regions = list(epi_df.subregion1_name)

    fig = make_subplots(rows=len(regions), cols=1,
                   shared_yaxes=True, shared_xaxes=True,
                   subplot_titles=regions,
                   vertical_spacing=0.001)

    for i in range(len(regions)):
        region = regions[i]
        i_table = (epi_df
                .query("subregion1_name==@region", engine="python")
                )
        population  = list(i_table.population)[0]
        annual_last = list(i_table.annual_inc_last)[0]
        cum_last    = list(i_table.cum_inc_last)[0]
        
        fig.append_trace(
            go.Bar(
                orientation='h',
                y=["Annualized<br>Incidence"], x=i_table.annual_inc_last,
                name="Annualized<br>Incidence",
                marker=dict(color="crimson", coloraxis="coloraxis"),
                hovertemplate="<b>"+region+"</b><br>" +
                                "Annualized Inc. : {:.2%}<br>".format(annual_last) +
                                "Population: {:,}".format(population),
            ),
            row=i+1, col=1
        )
        
        fig.append_trace(
            go.Bar(
                orientation='h',
                y=["Cumulative<br>Incidence"], x=i_table.cum_inc_last,
                name="Cumulative<br>Incidence", opacity=0.5,
                marker=dict(color="lightslategrey", coloraxis="coloraxis"),
                width=0.3,
                hovertemplate="<b>"+region+"</b><br>" +
                                "Cumulative Inc. : {:.2%}<br>".format(cum_last) +
                                "Population: {:,}".format(population),
            ),
            row=i+1, col=1
        )

    fig.for_each_annotation(lambda a: a.update(x=1.0))
    fig.for_each_annotation(lambda a: a.update(xanchor="right"))
    fig.for_each_annotation(lambda a: a.update(yanchor="top"))
    fig.for_each_annotation(lambda a: a.font.update(size=14))
    fig.update_yaxes(visible=False, showticklabels=False)
    # fig.update_xaxes(visible=True, showticklabels=True)

    fig.update_layout(height=np.max([len(regions)*60, 600]), template="plotly_dark",showlegend=False,
                        title='Regional Current Annualized Incidence' +
                                "<br><sub>Annualized incidence (Red) - Cumulative Incidence (Grey)</sub>",
                        bargap=0.0001, bargroupgap=0.0001
                    )
    max_axis_key = "xaxis{}".format(len(regions))
    fig.update_layout({max_axis_key:dict(
                            title='Incidence',
                            gridwidth=2,
                            tickformat=".2%",
                        )})

    return fig