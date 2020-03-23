import numpy as np
import pandas as pd
import math
import json

from bokeh.models.graphs import from_networkx
from bokeh.plotting import figure
from bokeh.resources import CDN
from bokeh.embed import autoload_static, json_item, file_html
from bokeh.tile_providers import get_provider, Vendors
from bokeh.models import ColumnDataSource, CustomJS, LabelSet, Plot,\
WheelZoomTool, FuncTickFormatter, Legend, CategoricalColorMapper,\
Range1d, MultiLine, Circle, HoverTool, PanTool, ResetTool,\
LogColorMapper,  LinearAxis, Grid
from bokeh.models.widgets import Slider, Button
from bokeh.models.glyphs import Patches
from bokeh.layouts import column, row, widgetbox, Spacer
from bokeh.palettes import Spectral4
from bokeh.palettes import Viridis6 as palette
from bokeh.io import output_notebook, show, export_svgs
from bokeh.resources import CDN
from bokeh.embed import autoload_static, json_item
from bokeh.layouts import gridplot


def merc_from_arrays(lats, lons):
    r_major = 6378137.000
    x = r_major * np.radians(lons)
    scale = x/lons
    y = 180.0/np.pi * np.log(np.tan(np.pi/4.0 + lats * (np.pi/180.0)/2.0)) * scale
    return (x, y)

def make_map_plot():
    confirmed = pd.read_csv('./static/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv', encoding = "ISO-8859-1")
    confirmed['status'] = 'confirmed'
    recovered = pd.read_csv('./static/csse_covid_19_time_series/time_series_19-covid-Recovered.csv', encoding = "ISO-8859-1")
    recovered['status'] = 'recovered'
    deaths = pd.read_csv('./static/csse_covid_19_time_series/time_series_19-covid-Deaths.csv', encoding = "ISO-8859-1")
    deaths['status'] = 'deaths'

    total = pd.concat([confirmed, recovered, deaths])

    print(total)
    
    # Time slider pane
    TOOLS = "pan,wheel_zoom,reset,hover,save"

    timeslider = Slider(start=1, end=46,
        value=1, step=1, title="Click to see how cases spread")
    playbutton = Button(label="► Play", button_type="success")
    timeline = range(timeslider.end)

    p = figure(
        tools=TOOLS,
        x_axis_location=None, y_axis_location=None,
    )

    # Make plot figure
    p.grid.grid_line_color = None
    p.hover.point_policy = "follow_mouse"
    x_min, y_min = merc_from_arrays(34,-130)
    x_max, y_max = merc_from_arrays(45,-60)
    pad = 0         
    tile_provider = get_provider(Vendors.CARTODBPOSITRON)
    #tile_provider = get_provider(Vendors.STAMEN_TONER_LABELS)
    p = figure(x_range=(x_min-pad, x_max+pad), y_range=(y_min-pad,y_max+pad),
               x_axis_type="mercator", y_axis_type="mercator", width=800, height=500, title='Confirmed and recovered cases')
    p.add_tile(tile_provider)
    p.toolbar.active_scroll = p.select_one(WheelZoomTool)
    p.title.text_font_size = '14pt'
    p.toolbar.logo = None
    p.toolbar_location = None
    p.hover.point_policy = "follow_mouse"

    '''
    # Define county data sources
    county_source_dict = {}
    dict_by_year = {}
    county_sources2000 = []
    county_source_df = pd.DataFrame()
    '''

    '''
    #for _c in states:
    for _c in [27,6,17,23,12,23,24,20,25,36,9,34,36,37,55,50,33,42,39,26,10,51]:
        for _y in years:
        #for _y in [2000,2001,2002,2003,2004,2005]:
            max_cases = max(CDC_df[CDC_df['STCODE']==_c][f"Cases{_y}"])
            for index, row in CDC_df[CDC_df['STCODE']==_c].iterrows():
                if row['geo_key'] in list(counties.keys()):
                    tmp=merc_from_arrays(np.array([float(_x) for _x in counties[row['geo_key']]['lats']]), np.array([float(_x) for _x in counties[row['geo_key']]['lons']]))
                    county_source_dict[ (row['geo_key'], _y) ] = ColumnDataSource(data=dict(
                            xs=[tmp[0]],
                            ys=[tmp[1]],
                            #fill=[row[f"Cases{_y}"]/(0.000005+max_cases+100)]
                            fill=[(0.45+row[f"Cases{_y}"])/100]
                        )
                    )
            dict_by_year[_y] = [v for (k,v) in county_source_dict.items() if _y in k]

    for source in dict_by_year[2010]:
        glyph = Patches(xs="xs", ys="ys", fill_color="#3D6873",line_color=None,fill_alpha="fill")
        p.add_glyph(source, glyph)
    '''

    vspace = Spacer(min_height=20,max_height=20)
    hspace = Spacer(min_width=40,max_width=40)


    #timeSliderCallback = CustomJS(args=dict(source=dict_by_year[2000], data_dict=dict_by_year, source_bar=stacks_by_year),
    timeSliderCallback = CustomJS(args=dict(),
        code="""
            var f = cb_obj.value
            for (var i = 0; i < source.length; i++) {
                var tmp_source = source[i]
                var data = tmp_source.data;
                //data['fill'] = [0];
                var tmpnew = data_dict[f]
                data['fill'] = (tmpnew[i]).data['fill']
                tmp_source.change.emit()
            }
        """)
    
    animateSlider = CustomJS(args=dict(),
        code="""
            console.log("Clicked button")
            timeslider.value = timeslider.start
            console.log("Set to beginning")
            step_length_ms = 1500
            num_steps = lengthYears+1
            inc = 1
            
            playback_interval = setInterval(function(){
                timeslider.value = timeslider.value+inc;
                console.log(timeslider.value);
            }, step_length_ms)
            setTimeout(function(){clearInterval(playback_interval)}, step_length_ms*num_steps)

        """)
    
    timeslider.js_on_change('value', timeSliderCallback)
    playbutton.js_on_click(animateSlider)

    m_layout = column(p, timeslider, widgetbox(playbutton, width=100))
    map_plot_json = json.dumps(json_item(m_layout, "map-plot-div"))
    #f= open("static/map_plot.json","w+")
    #f.write(map_plot_json)
    #f.close()
    return map_plot_json


def make_bar_plot():
    print('----------------------Making bar plot')
    '''
    df = pd.read_csv('data/Lyme_data.csv', encoding = "ISO-8859-1")
    trends = [(trend.split(',')) for trend in list(set(df['Trend']))]
    trends_individual = [j for i in trends for j in i]

    factors = []
    states = list(set(df['State']))
    [states.remove(x) for x in ['IL','KS','CO','AL','GA','FL']]
    years = list(set(df['Year']))
    
    categories = ["Awareness","Doctor Protection","Insurance","Vector Borne Illness Control","Treatment Standards","Education"]
    data_per_year = {}
    data = {'states' : states}

    plot_dict = {}
    #cur_year = 2018
    for _y in years:
        data = []
        data = {'states' : states}
        for _c in categories:
            data[_c] = np.zeros(len(states),dtype=int)
        for _c in categories:
            data[_c] = np.zeros(len(states),dtype=int)
            for ii,_st in enumerate(states):
                for i in df[(df['Year']==_y) & (df['State']==_st)].index:
                    _trends = (df.get_value(i,'Trend'))
                    if _c in _trends:
                        data[_c][ii]=(_trends.count(_c))
        data_per_year[_y] = data

        colors = ["#c9d9d3", "#718dbf", "#e84d60", "#3D6973", "#80485D", "#A68A94"]
        #p2 = figure(x_range=states, plot_height=200, plot_width=250, title=f"{_y}",
        #           toolbar_location=None, tools="hover")
        p2 = figure(x_range=states, plot_height=150, plot_width=250, title=f"{_y}",
                   toolbar_location=None)
        p2.vbar_stack(categories, x='states', width=0.9, color=colors, source=ColumnDataSource(data=data_per_year[_y]))
        p2.y_range.start = 0
        p2.y_range.end = 6
        p2.x_range.range_padding = 0.1
        p2.xgrid.grid_line_color = None
        p2.axis.minor_tick_line_color = None
        p2.outline_line_color = None
        p2.legend.orientation = "horizontal"
        p2.legend.location = "top_center"
        p2.legend.label_text_font = '2pt'
        p2.legend.glyph_width = 10
        p2.legend.glyph_height = 10
        p2.legend.label_width = 5
        p2.legend.label_text_line_height = 5
        p2.legend.visible = False
        plot_dict[_y] = p2
    p2010 = plot_dict[2010]
    p2012 = plot_dict[2012]
    p2013 = plot_dict[2013]
    p2014 = plot_dict[2014]
    grid = gridplot(children=[[plot_dict[2011], p2012], [p2013,p2014], [plot_dict[2015], plot_dict[2016]], [plot_dict[2017], plot_dict[2018]]],
        toolbar_location=None,toolbar_options={'logo': None})
    plot_json = json.dumps(json_item(grid, f"bar-plot-div"))
    f= open(f"static/bar_plot.json","w+")
    f.write(plot_json)
    f.close()
    '''