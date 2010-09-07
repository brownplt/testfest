var sessionId;


function saveSessionId() {
  var match =  document.cookie.match('session\=\"(.*)\"');
  if (match) { sessionId = match[1]; }
}

function isLoggedIn(k) {
  var match =  document.cookie.match('session\=\"(.*)\"');
  sessionId = match && match[1];
  if (!sessionId || sessionId === "expired") {
    k(false);
  }
  else {
    new Ajax.Request(sessionId + "?command=ping",
                     { onSuccess: function(t) { 
                       var isPong = false;
                       try {
                         isPong = t.responseText.evalJSON().value === "pong";
                       }
                       catch(_) { }
                       k(isPong);
                     },
                       onFailure: function(_) { k(false); }
                     });
  }
}

Date.shortMonths = ["Jan","Feb","Mar","Apr","May","Jun","Jul",
  "Aug","Sep","Oct","Nov","Dec"];

Date.prototype.formatRelative = function() {
  var now = new Date();

  if (this.getDay() == now.getDay() && this.getMonth() == now.getMonth() &&
      this.getFullYear() == now.getFullYear()) {
    return (this.getHours() + (this.getMinutes() <= 9 ? ":0" : ":") +
            this.getMinutes());
  }
  else if (this.getFullYear() == now.getFullYear()) {
    return (this.getDate() + " " + Date.shortMonths[this.getMonth()]);
  }
  else {
    return (this.getDate() + " " + Date.shortMonths[this.getMonth()] + " "
            + (this.getFullYear() % 100));
  }
};

var LINK = function(elt) { 
  return A({className: "links", href: 'javascript:undefined'},elt); 
};

var isSuccess = function(v) { return v.success; };

function server_e(url,valueE) {
  var response_e = receiver_e();

  var onSuccess = function(transport) {
    saveSessionId();
    var txt = transport.responseText.evalJSON(false);
    response_e.sendEvent(txt);
  };

  var onFailure = function(transport) {
    if (console && console.log) { console.log(transport); }
    var txt = (transport.responseText.length > 0) 
              ? transport.responseText : "Session Expired";
    modal(DIV({ className: 'shutdown'},
           txt,
            DIV(A({ style: { fontSize: '12px',color: '#ffffff' },
                    href: 'javascript:window.location.reload()'}, 
                'Log In Again'))),
      one_e(true),200);
  };

  valueE.lift_e(function(val) {
    if (sessionId) { val.session = sessionId; }
    new Ajax.Request(url,
      { method: 'post'
      , parameters: val
      , onSuccess: onSuccess
      , onFailure: onFailure
      });
  });

  return response_e;
};

// buttonBar :: buttons * buttons * button 
//           -> { elem: element, clickE: EventStream buttons }
// button = String
// buttons = Array button
//
// You must ensure that the button names are unique, or all hell will
// break loose.
function buttonBar(buttons,rightButtons,initial) {

  rightButtons = rightButtons || [ ];
  var last = false;

  var clickEs = [ ];
  var targetEs = [ ];

  var left = [ ];
  var right = [ ];

  left.push({className: 'buttonBar'});
 
  buttons.each(function(button) {
    var elem = DIV({className: 'button'},button.toString());
    var e = clicks_e(elem);
    targetEs.push(e);
    clickEs.push(e.constant_e(button));
    left.push(elem);
    if (button == initial) {
      last = elem;
       elem.setStyle({ backgroundColor: '#ffff99', color: '#000000' });
    }
  });

  right.push({className: 'right'});
  
  
  rightButtons.each(function(button) {
    var elem = DIV({className: 'button'},button);
    var e = clicks_e(elem);
    targetEs.push(e);
    clickEs.push(e.constant_e(button));
    right.push(elem);
    if (button == initial) {
      last = elem;
       elem.setStyle({ backgroundColor: '#ffff99', color: '#000000' });
    }
  });

  left.push(DIV.apply(this,right));

  var clickE = merge_e.apply(this,clickEs);
  var targetE = merge_e.apply(this,targetEs);

  targetE.lift_e(function(target) { 
    if (last) { last.setStyle({ backgroundColor: '', color: '' }); }
    last = target;
    last.setStyle({ backgroundColor: '#ffff99', color: '#000000' });
  });

  return { clickE: clickE
         , elem: DIV.apply(this,left) };
};

// panelB :: EventStream String * { String : Elem or (-> Elem) }  * Elem
//        -> Behaviour Elem
// Consumes an eventstream carrying panel names, a dictionary that maps
// panel names to either a panel element or a thunk that returns the panel 
// element and finally, the initial panel.  Returns a behavior carrying the
// current panel
//
// Use a thunk in the dictionary to perform any setup when a panel is swapped
// into the view, even if the layout is static.  It's amazing!
function panelB(panelE,panelDict,initialPanel) {
  if (!(initialPanel instanceof HTMLElement)) {
    return "error calling panelB: intialPanel is not an HTML element";
  }

  return panelE.lift_e(function(name) {
    var val = panelDict[name];
    if (typeof(val) == "function") { return val(); }
    else if (val instanceof HTMLElement) { return val; }
    else  { return false; /* panelE violated the contract, fail silently */ }
  }).filter_e(function(val) { return val instanceof HTMLElement; })
  .startsWith(initialPanel);
};


function modal(content,hideShowE,zIndex) {
  var whiteout = DIV({ style: { position: 'absolute', 
                                height: '100%',
                                width: '100%',
                                left: '0px',
                                top: '0px',
                                zIndex: 100,
                                backgroundColor: 'black' }});
  whiteout.setOpacity(0.3);
  document.body.appendChild(whiteout);
  
  var win = DIV( { style: { position: 'absolute'
                          , left: '25%'
                          , minWidth: '400px'
                          , top: '10%'
                          , zIndex: zIndex || 101
                          } }
               , content);

  document.body.appendChild(win);
  var hidden = false;

  hideShowE.lift_e(function(val) {
    if (!val && !hidden) {
      document.body.removeChild(whiteout);
      document.body.removeChild(win);
      hidden = true;
    }
  });

  return hideShowE;
};

