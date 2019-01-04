function h$newMouseTrackRef(element) {
  var ref = { x: -1, y: -1, b: 0, listeners: {}, element: element };

  ref.listeners.mousemove = function(event) {
    ref.x = event.offsetX;
    ref.y = event.offsetY;
  };

  ref.listeners.mouseup = function(event) {
    ref.b = event.buttons;
  };

  ref.listeners.mousedown = function(event) {
    ref.b = event.buttons;
  };

  ref.listeners.mouseover = function(event) {
    ref.x = event.offsetX;
    ref.y = event.offsetY;
    ref.b = event.buttons;
  };

  ref.listeners.mouseout = function(event) {
    ref.x = -1;
    ref.y = -1;
    ref.b = 0;
  };

  element.addEventListener('mousemove', ref.listeners.mousemove);
  element.addEventListener('mouseup'  , ref.listeners.mouseup  );
  element.addEventListener('mousedown', ref.listeners.mousedown);
  element.addEventListener('mouseover', ref.listeners.mouseover);
  element.addEventListener('mouseout' , ref.listeners.mouseout );

  return ref;
}

function h$disableMouseTrackRef(ref) {
  if (!ref.listeners) {
    console.log ("Extraneous call of h$disableMouseTrackRef");
    return;
  }

  element.removeEventListener('mousemove', ref.listeners.mousemove);
  element.removeEventListener('mouseup'  , ref.listeners.mouseup  );
  element.removeEventListener('mousedown', ref.listeners.mousedown);
  element.removeEventListener('mouseover', ref.listeners.mouseover);
  element.removeEventListener('mouseout' , ref.listeners.mouseout );

  ref.element = undefined;
  ref.listeners = undefined;

  ref.x = -1;
  ref.y = -1;
  ref.b = 0;
}
