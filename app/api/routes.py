import os
import platform
import xmltodict
from flask import Blueprint, Response
from app.api.standardize_output import standardize_lists

api = Blueprint('api', __name__, url_prefix='/api/v1')


@api.route('/analysis/<string:word>', methods=['GET'])
def analyze(word):
    out = ""
    
    if platform.system() == 'Linux':
        out = os.popen("cd /code/wordsjson/app/dist/bin && ./wordsxml " + word)

    elif platform.system() == 'Windows':
        out = os.popen("cd app/dist/WINNT_x86-gcc3 && wordsxml.exe " + word)

    elif platform.system() == 'Darwin':
        out = os.popen("cd app/dist/Darwin && wordsxml " + word)

    # parse response and return json
    out = out.read()
    resp = xmltodict.parse(out, dict_constructor=dict)
    resp = standardize_lists(resp)
    
    # for some reason "jsonify" doesn't work here and the response has to be converted to a string and the content-type set manually
    return Response(str(resp), mimetype='application/json')