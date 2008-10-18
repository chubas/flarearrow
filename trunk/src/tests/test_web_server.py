import unittest
import socket

def test_connection(method):
  def connect(host = 'localhost', port = 5000, resource = 'test_file.html'): 
    try:
      sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      sock.connect((host, port))
      sock_out = sock.makefile('wb')
      sock_in = sock.makefile('r')
      sock.close()
      print >> sock_out, method + ' /' + resource + ' HTTP/1.0\n\n'
      sock_out.close()
      result = sock_in.read()
      sock_in.close()
      return result
    except socket.error:
      print "\n(!) Could not connect to " + host + ":" + str(port) + \
            " for testing (" + method + ").\n" + \
            "\tMake sure the server is correctly configured and running."
      return ""
  return connect

test_connection_get     = test_connection("GET")
test_connection_post    = test_connection("POST")

def readfile(filename):
  f = open(filename, 'r')
  contents = f.read()
  f.close()
  return contents
  

class ClientTest(unittest.TestCase):
  def test_http_get(self):
    code = "HTTP/1.1 200 OK"
    content_type = "Content-type: text/html"
    content_length = "Content-Length: 416"
    server = "Server: Ocamlnet/2.2.8.1"
    contents = readfile("../../public/test_file.html")
    response = test_connection_get()

    self.assert_(code in response)
    self.assert_(content_type in response)
    self.assert_(content_length in response)
    self.assert_(server in response)
    self.assert_(contents in response)

  def test_http_post(self):
    code = "HTTP/1.1 200 OK"
    content_type = "Content-type: text/html"
    content_length = "Content-Length: 416"
    server = "Server: Ocamlnet/2.2.8.1"
    contents = readfile("../../public/test_file.html")
    response = test_connection_post()

    self.assert_(code in response)
    self.assert_(content_type in response)
    self.assert_(content_length in response)
    self.assert_(server in response)
    self.assert_(contents in response)

unittest.main()
