
from flask import Flask, render_template, jsonify,\
	request, json, send_from_directory
from bokeh.plotting import figure
from bokeh.embed import autoload_static, json_item, file_html

import os
import csv
from plots import make_map_plot, make_bar_plot

app = Flask(__name__)

@app.route('/', methods=['GET'])
def home():
	map_json = make_map_plot()
	make_bar_plot()
	return render_template("home.html")

@app.route('/favicon.ico') 
def favicon(): 
    return send_from_directory(os.path.join(app.root_path, 'static'), 'favicon.ico', mimetype='image/vnd.microsoft.icon')
    
if __name__ == '__main__':
    app.run(debug=True, port=5020)
