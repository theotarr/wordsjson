import os
import json
import platform
import xmltodict
import unidecode
from flask import Blueprint, Response
from app.api.standardize_output import standardize_lists

api = Blueprint('api', __name__, url_prefix='/api/v1')


@api.route('/la-to-en/<string:word>', methods=['GET'])
def analyze(word):
    if not word:
        return "Please provide a Latin word to analyze.", 400

    # strip special characters (ie. accents, long marks, etc.)
    word = unidecode.unidecode(word)

    out = ""
    # platform specific intrusions
    if platform.system() == 'Linux' or platform.system() == 'Darwin':
        out = os.popen("cd /code/wordsjson/app/src && ./wordsxml " + word)

    elif platform.system() == 'Windows':
        out = os.popen("cd app/src && wordsxml.exe " + word)

    # parse response and return json
    resp = xmltodict.parse(out.read(), dict_constructor=dict)
    resp = standardize_lists(resp)
    
    """
    For some reason Flask's "jsonify" doesn't work here, so the response has 
    to be converted to a valid JSON string and then converted to a Response object.
    """
    return Response(json.dumps(resp), mimetype='application/json'), 200


@api.get('/en-to-la/<string:word>')
def en_to_la(word):
    if not word:
        return "Please provide an English word to analyze.", 400

    # strip special characters (ie. accents, long marks, etc.)
    word = unidecode.unidecode(word)

    out = ""
    # platform specific intrusions
    if platform.system() == 'Linux' or platform.system() == 'Darwin':
        out = os.popen("cd /code/wordsjson/app/src && ./wordsxml ~e " + word)

    elif platform.system() == 'Windows':
        out = os.popen("cd app/src && wordsxml.exe ~e " + word)

    # parse response and return json
    resp = xmltodict.parse(out.read(), dict_constructor=dict)
    resp = standardize_lists(resp)
    return Response(json.dumps(resp), mimetype='application/json'), 200