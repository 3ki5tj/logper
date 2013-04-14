import os, sys
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from google.appengine.ext.webapp.util import run_wsgi_app

class MainPage(webapp.RequestHandler):
  def get(self, html="index.html"):
    self.response.headers['Content-Type'] = 'text/html'
    path = os.path.join(os.path.dirname(__file__), html)
    template_values = {
        'prim_ref': '''<h3>Primary reference</h3>
<ul>
<li>Cheng Zhang,
<i>Period cycles of the logistic map</i>,
<a href="http://arxiv.org/abs/1204.0546">arXiv:1204.0546</a>, 2012.
</li>
</ul>''',

        'go_home':
        '''<div class="linkenv">
        <ul class="links">
        <li class="linkhead">Links:
        <li><a href="/">Home</a>,
        <li><a href="/log">Logistic</a>,
        <li><a href="/cubic">Cubic</a>,
        <li><a href="/henon">H&eacute;non</a>
        </ul></div>''',

        'last_update':
        '<p>Last update: May. 7th, 2012.',
    }
    self.response.out.write(template.render(path, template_values))

class LogPage(MainPage):
  def get(self):
    MainPage.get(self, "log.html")

class CubicPage(MainPage):
  def get(self):
    MainPage.get(self, "cubic.html")

class HenonPage(MainPage):
  def get(self):
    MainPage.get(self, "henon.html")

class AboutPage(webapp.RequestHandler):
  def get(self):
    self.response.headers['Content-Type'] = 'text/plain'
    self.response.out.write('test page')

application = webapp.WSGIApplication(
  [('/', MainPage),
   ('/log', LogPage),
   ('/cubic', CubicPage),
   ('/henon', HenonPage),
   ('/about', AboutPage)],
  debug=True)

def main():
  run_wsgi_app(application)

if __name__ == "__main__":
  main()

