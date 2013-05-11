import os, sys
import webapp2, jinja2

JINJA_ENVIRONMENT = jinja2.Environment(
    loader=jinja2.FileSystemLoader(os.path.dirname(__file__)))


class MainPage(webapp2.RequestHandler):
  def get(self, fnhtml="index.html"):
    self.response.headers['Content-Type'] = 'text/html'

    template = JINJA_ENVIRONMENT.get_template(fnhtml)

    template_values = {
        'prim_ref': '''<h3>Primary reference</h3>
<ul>
<li>Cheng Zhang,
<i>Cycles of the logistic map</i>,
<a href="http://arxiv.org/abs/1204.0546">arXiv:1204.0546</a>, 2012.
</li>
</ul>''',

        'go_home':
        '''<div class="linkenv">
        <ul class="links">
        <li class="linkhead">Links:
        <li><a href="/">Home</a>,
        <li><a href="/logistic">Logistic</a>,
        <li><a href="/cubic">Cubic</a>,
        <li><a href="/henon">H&eacute;non</a>
        </ul></div>''',

        'last_update':
        '<p>Last updated on May 10th, 2013.',
    }
    self.response.write(template.render(template_values))


class LogisticPage(MainPage):
  def get(self):
    MainPage.get(self, "log.html")


class CubicPage(MainPage):
  def get(self):
    MainPage.get(self, "cub.html")


class HenonPage(MainPage):
  def get(self):
    MainPage.get(self, "hen.html")


class AboutPage(webapp2.RequestHandler):
  def get(self):
    self.response.headers['Content-Type'] = 'text/plain'
    self.response.write('test page')

app = webapp2.WSGIApplication(
  [('/', MainPage),
   ('/index', MainPage), # aliases
   ('/logistic', LogisticPage),
   ('/log', LogisticPage), # alias
   ('/cubic', CubicPage),
   ('/cub', CubicPage), # alias
   ('/henon', HenonPage),
   ('/hen', HenonPage), # alias
   ('/about', AboutPage)],
  debug=True)

