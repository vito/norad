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

  function connect(uri, settings) {
    return Task.asyncFunction(function(callback) {
      var es = new EventSource(uri);

      if (settings.onOpen.ctor === "Just") {
        es.onopen = function() {
          Task.perform(settings.onOpen._0._0(Utils.Tuple0));
        };
      }

      if (settings.onError.ctor === "Just") {
        es.onerror = function(e) {
          var readyState;

          switch (e.currentTarget.readyState) {
            case 0:
              readyState = { ctor: "Connecting" };
            case 1:
              readyState = { ctor: "Open" };
            case 2:
              readyState = { ctor: "Closed" };
          }

          Task.perform(settings.onError._0._0(readyState));
        };
      }

      callback(Task.succeed(es));
    });
  }

  function on(eventName, address, es) {
    return Task.asyncFunction(function(callback) {
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
    connect: F2(connect),
    on: F3(on),
    close: close,
  };

  return localRuntime.Native.EventSource.values;
};
