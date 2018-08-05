# wacko
Wacko is another Erlang web framework built ontop of Psycho which you can find [here](https://github.com/gar1t/psycho).

Wacko is not a piece of software made to be used for anything serious and is the result of playing around and experimentation.

## Current Features
As of now, there the bare minimum of features have been implemented:
- Automatic loading of static assets, views and controllers
- Automatically routing to controllers based on requested URL
- Automatically re-compiling controllers when they've changed
- Automatically fetching request bodies, further abstracting from Psycho

## Usage Instructions
If you want to just built atop Wacko, you can simply perform a ```git clone``` of this repo and start building stuff! Otherwise you can pull Wacko as a dependency by adding it to your ```rebar.config``` file in a typical rebar3 project.

By default, Wacko runs on port 8001 but this can be configured via sys.config as follows:

```erlang
[{wacko, [
    {port, default} % replace this with any valid port such as 8080, 443 or whatever you need.
]}].
```

As well as configuring the port Wacko runs on, Wacko's project directly (by default ```$WACKO_DIR/priv```) can be configured with via:

```erlang
[{wacko, [
    {project_dir, default} % replace this with a valid filename string
]}].
```

By default, static assets are stored under ```/priv/assets/```. Any request to some url such as ```localhost:8001/assets/something.png``` will just therefore simply be served up by Wacko.

Any other routes will be routed to a controller, and then a function as follows:
- A request such as ```localhost:8001``` will be routed to the controller ```priv/controllers/index.erl``` and the function ```index:index/3``` will be invoked. Such a function should have the type signature ```index("GET", Env, Args)``` for instance, where we perform pattern matching on any GET requests to that URL.
- A request such as ```localhost:8001/${controller}``` will be routed to the controller ```priv/controllers/${controller}.erl```, and the function, just like in the base case, ```${controller}:index/3``` will be envoked.
- A request such as ```localhost:8001/${controller}/${function}``` will be routed to the controller ```priv/controllers/${controller}.erl```, and the function ```${controller}:${function}/3``` will be invoked.
- Any other URI segments will be stored as arguments to be provided as the final parameter of the envoked function.

If a particular function is not expected any request body, then it is of arity 3 with the function signature ```function(Method, Env, Args)``` where the Method is the HTTP method (i.e. ```GET```, ```POST``` etc), Env is data associated with the request, containing things such as the request headers, and Args is a list of other URL slugs which made up the request.

If a particular function does contain a request body though, Wacko will envoke an arity 4 function instead, with the signature ```function(Method, Env, RequestBody, Args)```. Wacko decides this by looking for a non-zero ```content_length``` sent in the request header.

If the project dir isn't set to ```default``` or ```$WACKO_DIR/priv``` then the instructions above still apply, just relative to the project directory instead of the ```priv/``` directory.

### Returning a response
Ultimately, controllers need to return headers when complete which will then get sent back to the requester. These are in the form ```{{HTTP_CODE, HTTP_CODE_DESCRIPTION}, RESPONSE_HEADERS, RESPONSE_BODY}```. Wacko provides utility functions to automatically generate common responses as listed below:
- ```wacko:ok_html(Document)``` where document is the contents of the HTML document intended to be returned to the requester. This function sets the content_type to "text/html" and responds with a {200, ok} HTTP code. Variants for sending 404 and 400 errors also exist as ```wacko:not_found_html(Document)``` and ```wacko:bad_request_html(Document)``` respectively.
- ```wacko:ok(Headers, Data)```, ```wacko:not_found(Headers, Data)``` and ```wacko:bad_request(Headers, Data)``` exist to send custom headers and data as well. Arity 1 versions of these functions also exist, which default to no special headers being set and simply return data.
- Custom responses can be made by manually implemented a return tuple, or implementing your own functions but the function ```wacko_http:response({HTTP_CODE, HTTP_DESC}, HEADERS, DATA)``` also exists and is exported for use.

### Views
Views are stored in ```priv/views``` and currently, nothing special is done with them. You can load them via ```wacko:fetch_view(View_name)``` or ```wacko_view:fetch(View_name)``` but nothing special is happening there but us making a nice interface around the standard ```file:read_file/1``` function. You could easily use another library which is more fully featured to render templates if you need the functionality.

### Misc
Most common functions are exported by ```src/wacko.erl``` which simply acts as an interface for other modules. I'm unsure if this is great practice but as a result do look at the exports list of that file to figure anything else out :-)

## Upcoming Features
- Logging intergration and metrics
- Clean up code surrounding intercepting requests which need to be dispatched to arity 4 controller functions.
- Permissions for assets?
