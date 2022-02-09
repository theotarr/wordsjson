import os
from flask import Flask, make_response
import xmltodict


app = Flask(__name__)


@app.route('/api/v1/analysis/<string:word>', methods=['GET'])
def analyze(word):
    return analyze_word(word)


def analyze_word(word):
    # get the output of the command
    out = os.popen("cd wordsxml/dist/WINNT_x86-gcc3 && wordsxml.exe " + word)
    out = xml_to_json(out.read())

    return out

def xml_to_json(xml):
    # convert xml to json
    json = xmltodict.parse(xml)
    return json

    

    

if __name__ == '__main__':
    app.run(debug=True)