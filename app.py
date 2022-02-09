import os
from flask import Flask, make_response
import xmltodict

app = Flask(__name__)
@app.route('/api/v1/analysis/<string:word>', methods=['GET'])
def analyze(word):
    out = os.popen("cd /code/wordjson/dist/bin && ./wordsxml " + word)
    return xmltodict.parse(out.read())

if __name__ == '__main__':
    app.run(host='0.0.0.0')