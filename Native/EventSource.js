Elm.Native.EventSource = {};
Elm.Native.EventSource.make = function(localRuntime) {
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.EventSource = localRuntime.Native.EventSource || {};
  if (localRuntime.Native.EventSource.values) {
    return localRuntime.Native.EventSource.values;
  }

  var Task = Elm.Native.Task.make (localRuntime);
  var Maybe = Elm.Maybe.make(localRuntime);
  var Utils = Elm.Native.Utils.make (localRuntime);

  function connect(uri) {
    var es;
    return Task.asyncFunction(function(callback) {
      es = es || new EventSource(uri);
      if (es.connected) {
        callback(Task.succeed(es));
      } else {
        attemptConnection(es, callback);
      }
    });
  }

  function attemptConnection(es, callback) {
    var sent = false;

    es.onopen = function() {
      if (!sent) {
        sent = true;
        callback(Task.succeed(es));
      }
    };
  }

  function on(eventName, address, es) {
    return Task.asyncFunction(function(callback) {
      if (eventName === "") {
        eventName = "message";
      }

      es.addEventListener(eventName, function(event) {
        Task.perform(address._0({
          ctor: "Event",
          lastEventId: event.lastEventId ? Maybe.Just(event.lastEventId) : Maybe.Nothing,
          name: event.type ? Maybe.Just(event.type) : Maybe.Nothing,
          data: event.data
        }));
      });

      callback(Task.succeed(es));
    });
  }

  function close(es) {
    return Task.asyncFunction(function(callback) {
      es.close();
      callback(Task.succeed(Utils.Tuple0));
    });
  }

  localRuntime.Native.EventSource.values = {
    connect: connect,
    on: F3(on),
    close: close,
  };

  return localRuntime.Native.EventSource.values;
};
