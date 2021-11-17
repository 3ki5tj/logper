import os, sys
import webapp2, jinja2



JINJA_ENVIRONMENT = jinja2.Environment(
    loader=jinja2.FileSystemLoader(os.path.dirname(__file__)))



class MainPage(webapp2.RequestHandler):

  def get(self, fnhtml="index.html", title="Homepage"):
    self.response.headers['Content-Type'] = 'text/html'

    template = JINJA_ENVIRONMENT.get_template(fnhtml)

    template_values = {
        'header_html': '''<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=no">
<title>%s</title>
<script type="text/javascript" src="js/main.js?v=0.1.1"></script>
<link type="text/css" rel="stylesheet" href="/css/main.css?v=0.1.1">
</head>
''' % (title),
        'prim_ref': '''<h3>Primary reference</h3>
<ul>
<li>Cheng Zhang,
<i>Cycles of the logistic map</i>,
<a href="http://www.worldscientific.com/doi/abs/10.1142/S0218127414500059">
International Journal of Bifurcation and Chaos
<b>24</b> (2014) 1450005</a>;
also, an earlier preprint:
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
        <li><a href="/henon">H&eacute;non</a>,
        <li><a href="/esd">ESD</a>
        </ul></div>''',

        'last_update':
        '<p>Last updated on April 2nd, 2014.',
    }
    self.response.write(template.render(template_values))


class IndexPage(MainPage):
  def get(self):
    MainPage.get(self, "index.html", "Cycles of polynomial maps")


class LogisticPage(MainPage):
  def get(self):
    MainPage.get(self, "logistic.html", "Cycles of the logistic map")


class CubicPage(MainPage):
  def get(self):
    MainPage.get(self, "cubic.html", "Cycles of the antisymmetric cubic map")


class HenonPage(MainPage):
  def get(self):
    MainPage.get(self, "henon.html", "Cycles of the Henon map")


class ESDPage(MainPage):
  def get(self):
    MainPage.get(self, "esd.html", "Elementary Symbolic Dynamics")


class AboutPage(webapp2.RequestHandler):
  def get(self):
    self.response.headers['Content-Type'] = 'text/plain'
    self.response.write('test page')


app = webapp2.WSGIApplication(
  [('/', IndexPage),
   ('/index', IndexPage), # aliases

   ('/logistic', LogisticPage),
   ('/log', LogisticPage), # alias

   ('/cubic', CubicPage),
   ('/cub', CubicPage), # alias

   ('/henon', HenonPage),
   ('/hen', HenonPage), # alias

   ('/esd', ESDPage),

   ('/about', AboutPage)],
  debug=True)

