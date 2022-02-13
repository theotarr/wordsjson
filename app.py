import os
import platform
import xmltodict
from flask import Flask, Response
from util.standardize_output import standardize_lists
app = Flask(__name__)


@app.route('/api/v1/analysis/<string:word>', methods=['GET'])
def analyze(word):
    out = ""
    
    if platform.system() == 'Linux':
        out = os.popen("cd /code/wordsjson/dist/bin && ./wordsxml " + word)

    elif platform.system() == 'Windows':
        out = os.popen("cd dist/WINNT_x86-gcc3 && wordsxml.exe " + word)

    # parse response and return json
    out = out.read()
    resp = xmltodict.parse(out, dict_constructor=dict)
    resp = standardize_lists(resp)
    
    # for some reason "jsonify" doesn't work here and the response has to be converted to a string and the content-type set manually
    return Response(str(resp), mimetype='application/json')



@app.route('/test')
def test():
    return("Hello World")


if __name__ == '__main__':
    app.run(debug=True)