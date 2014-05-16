If you have leiningen installed:

git clone https://github.com/karolmajta/clojure-trends.git
cd clojure-trends
cat <path-to-input.csv> | lein run <n>

The program reads data from stdin, and spits n results to stout.
