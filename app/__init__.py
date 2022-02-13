from flask import Flask
from .config import DevConfig, ProdConfig, StagingConfig


def create_app():
    app = Flask(__name__)
    app.config.from_object(DevConfig)

    # register blueprints
    from .api import api as api_blueprint
    app.register_blueprint(api_blueprint)

    return app