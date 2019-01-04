function h$newKeyboardTrackRef() {
  var ref = { map: {}, listeners: {} };

  ref.listeners.keydown = function(event) {
    ref.map['keyCode:' + event.keyCode] = true;
  };

  ref.listeners.keyup = function(event) {
    delete ref.map['keyCode:' + event.keyCode];
  };

  document.addEventListener('keydown', ref.listeners.keydown);
  document.addEventListener('keyup',   ref.listeners.keyup  );

  return ref;
}

function h$disableKeyboardTrackRef(ref) {
  if (!ref.listeners) {
    console.log ("Extraneous call of h$disableKeyboardTrackRef");
    return;
  }

  document.removeEventListener('keydown', ref.listeners.keydown);
  document.removeEventListener('keyup',   ref.listeners.keyup  );

  ref.listeners = undefined;
  ref.map = {}
}

function h$readKeyboardTrackRef(ref) {
  // we can consider the returned value immutable.
  return Object.assign({}, ref.map);
}
