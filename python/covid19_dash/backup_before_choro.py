# myapp.py

from random import random
import numpy as np
import pandas as pd
import math
import json
import io
import requests

from bokeh.palettes import RdYlBu3
from bokeh.models.graphs import from_networkx
from bokeh.plotting import figure, curdoc
from bokeh.resources import CDN
from bokeh.embed import autoload_static, json_item, file_html
from bokeh.tile_providers import get_provider, Vendors
from bokeh.models import ColumnDataSource, CustomJS, LabelSet, Plot,\
WheelZoomTool, FuncTickFormatter, Legend, CategoricalColorMapper,\
Range1d, MultiLine, Circle, HoverTool, PanTool, ResetTool,\
LogColorMapper,  LinearAxis, Grid, Label, Title
from bokeh.models.widgets import Slider, Button, TextInput
from bokeh.models.glyphs import Patches
from bokeh.layouts import column, row, widgetbox, Spacer
from bokeh.palettes import Spectral4
from bokeh.palettes import Viridis6 as palette
from bokeh.io import output_notebook, show, export_svgs
from bokeh.resources import CDN
from bokeh.embed import autoload_static, json_item
from bokeh.layouts import gridplot
from bokeh.sampledata.us_counties import data as counties

def merc_from_arrays(lats, lons):
    r_major = 6378137.000
    x = r_major * np.radians(lons)
    scale = x/lons
    y = 180.0/np.pi * np.log(np.tan(np.pi/4.0 + lats * (np.pi/180.0)/2.0)) * scale
    return (x, y)


#################################################### GET AND CLEAN DATA ####################################################

s = requests.get('http://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv', verify=False).content
confirmed = pd.read_csv(io.StringIO(s.decode('utf-8')))
confirmed['status'] = 'confirmed'
confirmed['color'] = 'orange'

s = requests.get('http://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv', verify=False).content
recovered = pd.read_csv(io.StringIO(s.decode('utf-8')))
recovered['status'] = 'recovered'
recovered['color'] = 'green'

s = requests.get('http://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv', verify=False).content
deaths = pd.read_csv(io.StringIO(s.decode('utf-8')))
deaths['status'] = 'deaths'
deaths['color'] = 'red'
total = pd.concat([confirmed, recovered, deaths])
US_total = total[total['Country/Region']=='US']
print(total)

#### Separate US data
print(US_total)
print(US_total['Province/State'])


skipCols = 3
#print(f'size: {total.shape}')
numDays = total.shape[1]-6
#print(f'numDays: {numDays}')
latestDateCol = total.columns[numDays + skipCols]
#print(f'Date: {latestDateCol}')
#print(f"Size: { total.loc[total['status'] =='confirmed'][latestDateCol] }")


def update_data(attrname, old, new):
    dates = list(total.columns)
    dates = dates[skipCols:]
    latLong_confirmed = merc_from_arrays(total.loc[total['status'] =='confirmed']['Lat'], total.loc[total['status'] =='confirmed']['Long'])
    latLong_recovered = merc_from_arrays(total.loc[total['status'] =='recovered']['Lat'], total.loc[total['status'] =='recovered']['Long'])
    latLong_deaths = merc_from_arrays(total.loc[total['status'] =='deaths']['Lat'], total.loc[total['status'] =='deaths']['Long'])
    
    map_source_confirmed.data =dict(x=latLong_confirmed[0], y=latLong_confirmed[1], 
        size=10*np.log10(total.loc[total['status'] =='confirmed'][dates[timeslider.value]].replace(to_replace=0, value=1)))
    map_source_recovered.data =dict(x=latLong_recovered[0], y=latLong_recovered[1], 
        size=10*np.log10(total.loc[total['status'] =='recovered'][dates[timeslider.value]].replace(to_replace=0, value=1)))
    map_source_deaths.data =dict(x=latLong_deaths[0], y=latLong_deaths[1], 
        size=10*np.log10(total.loc[total['status'] =='deaths'][dates[timeslider.value]].replace(to_replace=0, value=1)))

    print(f'Current date: {dates[timeslider.value]}')


#################################################### MAP PLOT ####################################################
# Choropleth
counties = { code: county for code, county in counties.items() }

county_lons = [county["lons"] for county in counties.values()]
county_lats = [county["lats"] for county in counties.values()]
county_names = [county['name'] for county in counties.values()]

US_latLong_confirmed = merc_from_arrays(total.loc[total['status'] =='confirmed']['Lat'], total.loc[total['status'] =='confirmed']['Long'])
US_map_source_confirmed = ColumnDataSource(data=dict(x=latLong_confirmed[0], y=latLong_confirmed[1], size=10*np.log10(total.loc[total['status'] =='confirmed'][latestDateCol])))

US_latLong_recovered = merc_from_arrays(total.loc[total['status'] =='recovered']['Lat'], total.loc[total['status'] =='recovered']['Long'])
US_map_source_recovered = ColumnDataSource(data=dict(x=latLong_recovered[0], y=latLong_recovered[1], size=10*np.log10(total.loc[total['status'] =='recovered'][latestDateCol])))

US_latLong_deaths = merc_from_arrays(total.loc[total['status'] =='deaths']['Lat'], total.loc[total['status'] =='deaths']['Long'])
US_map_source_deaths = ColumnDataSource(data=dict(x=latLong_deaths[0], y=latLong_deaths[1], size=10*np.log10(total.loc[total['status'] =='deaths'][latestDateCol])))

for key, value in counties.items() :
    print(value['name'])

tmp = []
for i in range(50):
    tmp = merc_from_arrays( np.array([float(_x) for _x in county_lats[50]]), np.array([float(_x) for _x in county_lons[50]]))
county_xs = tmp[0]
county_ys = tmp[1]

#glyph = Patches(xs="xs", ys="ys", fill_color="#3D6873",line_color=None,fill_alpha="fill")
#p.add_glyph(source, glyph)

TOOLS = "pan,wheel_zoom,reset,hover,save"
p = figure(
    tools=TOOLS, toolbar_location=None,
    x_axis_location=None, y_axis_location=None,
    x_axis_type="mercator", y_axis_type="mercator", width=1600, height=800
)
p.add_layout(Title(text="source: JHU CSSE", text_font_style="italic"), 'above')
p.add_layout(Title(text="Confirmed cases, recovered cases, and deaths of COVID-19", text_font_size="16pt"), 'above')
p.border_fill_color = 'white'
p.background_fill_color = 'white'
p.outline_line_color = None
p.grid.grid_line_color = None
p.grid.grid_line_color = None
#p.hover.point_policy = "follow_mouse"
        
tile_provider = get_provider(Vendors.CARTODBPOSITRON)
#tile_provider = get_provider(Vendors.STAMEN_TONER)
p.add_tile(tile_provider)
p.toolbar.active_scroll = p.select_one(WheelZoomTool)
p.title.text_font_size = '14pt'
p.toolbar.logo = None
p.toolbar_location = None
p.hover.point_policy = "follow_mouse"
p.scatter('x', 'y', 'size', source=map_source_confirmed, fill_alpha=0.3, line_alpha=1, fill_color='orange', line_color='orange')
p.scatter('x', 'y', 'size', source=map_source_deaths, fill_alpha=0.3, line_alpha=1, fill_color='red', line_color='red')
p.scatter('x', 'y', 'size', source=map_source_recovered, fill_alpha=0.3, line_alpha=1, fill_color='green', line_color='green')

timeslider = Slider(start=1, end=numDays,
    value=numDays, step=1, title="Days since data collection")
timeslider.on_change('value', update_data)
playbutton = Button(label="► Play", button_type="success")
timeline = range(timeslider.end)


citation = Label(x=70, y=70, x_units='screen', y_units='screen',
                 text=f'{total.columns[timeslider.value+3]}', render_mode='css',
                 border_line_color='black', border_line_alpha=1.0,
                 background_fill_color='white', background_fill_alpha=1.0)
p.add_layout(citation)

#for key, value in counties.items() :
#    print(key)


p.patches('xs','ys', source=ColumnDataSource(data=dict(
                            xs=[tmp[0]],
                            ys=[tmp[1]])), fill_alpha=0, line_width=0.3, line_color='black')



animateSlider = CustomJS(args=dict(timeslider=timeslider, end=numDays),
        code="""
            console.log("Clicked button")
            timeslider.value = timeslider.start
            console.log("Set to beginning")
            step_length_ms = 300
            num_steps = end-1
            inc = 1
            
            playback_interval = setInterval(function(){
                timeslider.value = timeslider.value+inc;
                console.log(timeslider.value);
            }, step_length_ms)
            setTimeout(function(){clearInterval(playback_interval)}, step_length_ms*num_steps)

        """)
    
playbutton.js_on_click(animateSlider)



#################################################### BAR PLOT ####################################################

fruits = ['Apples', 'Pears', 'Nectarines', 'Plums', 'Grapes', 'Strawberries']
counts = [5, 3, 4, 2, 4, 6]

barplot = figure(x_range=fruits, plot_height=250, title="Fruit Counts",
           toolbar_location=None, tools="")

barplot.vbar(x=fruits, top=counts, width=0.9)

barplot.xgrid.grid_line_color = None
barplot.y_range.start = 0




#################################################### LAYOUT AND ADD TO HMTL DOC ####################################################

#m_layout = column(p, timeslider, widgetbox(playbutton, width=100), row(barplot))
m_layout = column(p, timeslider, widgetbox(playbutton, width=100))
curdoc().add_root(column(row(m_layout)))
curdoc().title = "COVID-19 Dash"