import sys
     import os
     sys.path.append(os.path.dirname(os.path.abspath(__file__)))
     from src.cli.cli import cli

     if __name__ == "__main__":
         cli()
