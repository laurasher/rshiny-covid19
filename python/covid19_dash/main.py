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
#total = total[total['Country/Region']!='US']
print(total)


skipCols = 3
scaling = 30
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

    US_map_source_confirmed.data = dict(xs=[patch_dict['Los Angeles, CA']['xs']],
                                                     ys=[patch_dict['Los Angeles, CA']['ys']],
                                                     fill_alpha=(US_total[(US_total['Province/State']=='Los Angeles, CA') & (US_total['status']=='confirmed')][dates[timeslider.value]])/scaling,
                                                     fill_color=US_total[(US_total['Province/State']=='Los Angeles, CA') & (US_total['status']=='confirmed')]['color'])
    date_label_source.data = dict(names=[dates[timeslider.value]])
    #print(f'Current date: {dates[timeslider.value]}')


#################################################### MAP PLOT ####################################################

latLong_confirmed = merc_from_arrays(total.loc[total['status'] =='confirmed']['Lat'], total.loc[total['status'] =='confirmed']['Long'])
map_source_confirmed = ColumnDataSource(data=dict(x=latLong_confirmed[0], y=latLong_confirmed[1], size=10*np.log10(total.loc[total['status'] =='confirmed'][latestDateCol])))

latLong_recovered = merc_from_arrays(total.loc[total['status'] =='recovered']['Lat'], total.loc[total['status'] =='recovered']['Long'])
map_source_recovered = ColumnDataSource(data=dict(x=latLong_recovered[0], y=latLong_recovered[1], size=10*np.log10(total.loc[total['status'] =='recovered'][latestDateCol])))

latLong_deaths = merc_from_arrays(total.loc[total['status'] =='deaths']['Lat'], total.loc[total['status'] =='deaths']['Long'])
map_source_deaths = ColumnDataSource(data=dict(x=latLong_deaths[0], y=latLong_deaths[1], size=10*np.log10(total.loc[total['status'] =='deaths'][latestDateCol])))

#################################################### Choropleth
counties = { code: county for code, county in counties.items() }

US_county_confirmed = US_total.loc[US_total['status'] =='confirmed']['Province/State']
US_county_recovered = US_total.loc[US_total['status'] =='recovered']['Province/State']
US_county_deaths = US_total.loc[US_total['status'] =='deaths']['Province/State']

# Join county patches to case count data by joining on county key
print(f'---------------US_county_confirmed, {US_county_confirmed}')
county_df = pd.DataFrame.from_dict(counties, orient='index')
county_df['state'] = county_df['state'].str.upper()
county_df['Province/State'] = county_df['name'] + ', ' + county_df['state']
county_join_df = US_total.join(county_df.set_index('Province/State'), on='Province/State')

'''
patch_df = pd.DataFrame({'Province/State':[], 'xs':[], 'ys':[]})
patch_dict = {}
for index, row in county_join_df[county_join_df['lats'].notna()].iterrows():
    print(index)
    tmp = merc_from_arrays(np.array([float(_x) for _x in row['lats']]), np.array([float(_x) for _x in row['lons']]))
    patch_df = patch_df.append({'Province/State': row['Province/State'], 'xs': tmp[0], 'ys': tmp[1]}, ignore_index=True)
    patch_dict[row['Province/State']] = {'xs':list(tmp[0]), 'ys':list(tmp[1])}

print(patch_dict)
json = json.dumps(patch_dict)
f = open("dict.json","w")
f.write(json)
f.close()
county_join_df = county_join_df.join(patch_df.set_index('Province/State'), on='Province/State')
county_join_df = county_join_df.drop(['lats','lons','state'], axis=1)


US_latLong_confirmed = county_join_df.loc[county_join_df['status'] =='confirmed']['xs'], county_join_df.loc[county_join_df['status'] =='confirmed']['ys']
US_latLong_recovered = county_join_df.loc[county_join_df['status'] =='recovered']['Lat'], county_join_df.loc[county_join_df['status'] =='recovered']['Long']
US_latLong_deaths = county_join_df.loc[county_join_df['status'] =='deaths']['Lat'], county_join_df.loc[county_join_df['status'] =='deaths']['Long']
'''

with open('dict.json') as json_file:
    patch_dict = json.load(json_file)
print(patch_dict.keys())
#print([ item['xs'] for item in patch_dict.values() ])
print('-------><--------')
print(US_total[(US_total['Province/State'] == 'Los Angeles, CA') & (US_total['status']=='confirmed')])

US_map_source_confirmed = ColumnDataSource(data=dict(xs=[ item['xs'] for item in patch_dict.values() ],
                                                     ys=[ item['ys'] for item in patch_dict.values() ],
                                                     fill_alpha=(US_total[(US_total['Province/State'] == 'Los Angeles, CA') & (US_total['status']=='confirmed')][latestDateCol])/scaling,
                                                     fill_color=US_total[(US_total['Province/State'] == 'Los Angeles, CA') & (US_total['status']=='confirmed')]['color']))

US_map_source_recovered = ColumnDataSource(data=dict(x=latLong_recovered[0],
                                                     y=latLong_recovered[1],
                                                     fill=10*np.log10(total.loc[total['status'] =='recovered'][latestDateCol])))
US_map_source_deaths = ColumnDataSource(data=dict(x=latLong_deaths[0],
                                                  y=latLong_deaths[1],
                                                  size=10*np.log10(total.loc[total['status'] =='deaths'][latestDateCol])))

date_label_source = ColumnDataSource(data=dict(names=[latestDateCol]))



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
        
tile_provider = get_provider(Vendors.CARTODBPOSITRON)
#tile_provider = get_provider(Vendors.STAMEN_TONER)
p.add_tile(tile_provider)
p.toolbar.active_scroll = p.select_one(WheelZoomTool)
p.title.text_font_size = '14pt'
p.toolbar.logo = None
p.toolbar_location = None
#p.hover.point_policy = "follow_mouse"
p.scatter('x', 'y', 'size', source=map_source_confirmed, fill_alpha=0.3, line_alpha=1, fill_color='orange', line_color='orange', legend_label='confirmed cases')
p.scatter('x', 'y', 'size', source=map_source_deaths, fill_alpha=0.3, line_alpha=1, fill_color='red', line_color='red', legend_label='deaths')
p.scatter('x', 'y', 'size', source=map_source_recovered, fill_alpha=0.3, line_alpha=1, fill_color='green', line_color='green', legend_label='recovered')

glyph = Patches(xs="xs", ys="ys", fill_alpha="fill_alpha", fill_color="fill_color", line_color=None)
p.add_glyph(US_map_source_confirmed, glyph)
p.legend.click_policy="hide"
timeslider = Slider(start=1, end=numDays,
    value=numDays, step=1, title="Days since data collection")
timeslider.on_change('value', update_data)
playbutton = Button(label="► Play", button_type="success")
timeline = range(timeslider.end)


#citation = Label(text=f"{latestDateCol}", x=70, y=70, x_units='screen', y_units='screen', render_mode='css',
#                 border_line_color='black', border_line_alpha=1.0,
#                 background_fill_color='white', background_fill_alpha=1.0)
#p.add_layout(citation)


labels = LabelSet(x=70, y=70, x_units='screen', y_units='screen', text='names', level='glyph',
              x_offset=5, y_offset=5, source=date_label_source, render_mode='css', border_line_color='white', border_line_alpha=0.0,
              text_font_size = '34px', background_fill_color='white', background_fill_alpha=0.0)
p.add_layout(labels)

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