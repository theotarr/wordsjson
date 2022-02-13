import os
#from dotenv import load_dotenv

#load_dotenv()

class Config:
    DEVELOPMENT = False
    DEBUG = False
    CSRF_ENABLED = True

    # Set up the App SECRET_KEY
    SECRET_KEY = "bigsecret" #os.getenv('SECRET_KEY')

    # Set up the database
    # SQLALCHEMY_DATABASE_URI = os.getenv('DB_URI')
    # SQLALCHEMY_DATABASE_URI = SQLALCHEMY_DATABASE_URI.replace('postgres://', 'postgresql://')
    # SQLALCHEMY_TRACK_MODIFICATIONS = False


class ProdConfig(Config):
    FLASK_ENV = 'production'
    DEVELOPMENT = False
    DEBUG = False
    

class StagingConfig(Config):
    FLASK_ENV = 'staging'
    DEVELOPMENT = True
    DEBUG = True


class DevConfig(Config):
    FLASK_ENV = 'development'
    DEVELOPMENT = True
    DEBUG = True
