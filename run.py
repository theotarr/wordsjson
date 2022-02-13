from app import create_app

def run_words():
    app = create_app()
    app.run
    
if __name__ == "__main__":
    run_words()
    
