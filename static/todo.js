
var csrfKey = null;
var auth = null;
var userId = null;
var user = null;

function error(x, t, e) {
  alert("ERROR!");
}

function sendRaw(json, f) {
  $.ajax("/api", {
    dataType: "json",
    contentType: "application/json",
    processData: false,
    data: JSON.stringify(json),
    type: 'POST',
    success: f,
    error: error
  });
}

function send(json, f) {
  // TODO delay sending until we have a csrfKey
  // This could work by having send defined as adding the thing to a queue,
  // and having the csrfKey callback redefining send as this function.
  sendRaw({"Key": csrfKey, "Data": json}, function(r) {
    if (r.Data.RespError) {
      // TODO something better
      alert("Error: " + JSON.stringify(r.Data.RespError.Type));
    } else {
      f(r.Data);
    }
  });
}

function getCurUser() {
  send({"ReqCurUser": []}, function(r) {
    if (r.RespUserProfile) {
      userId = r.RespUserProfile[0];
      user = r.RespUserProfile[1];
      $("#name").text(user.Name);
      $("#main").pages("go");
    } else {
      $("#auth").pages("go");
    }
  });
}

var F = document.forms;

$(document).ready(function() {
  sendRaw({"Key": "", "Data": null}, function(r) {
    csrfKey = r.Key;
    getCurUser();
  });
  $("body > .page").pages("setup");
  $("#pages > .page").pages("setup");
  $("#entries").pages("go");
  $(F.Login).submit(function() {
    send({"ReqLogIn": {
      "User": F.Login.User.value,
      "Password": F.Login.Password.value}}, getCurUser);
    return false;
  });
  $(F.register).submit(function() {
    if (F.login.password.value == F.register.password.value) {
      send({"ReqRegister": {
        "User": F.login.user.value,
        "Password": F.login.password.value}}, getCurUser);
    }
    return false;
  });
  $("#time").keyup(function() {
    send({"ReqParseTime": {"Input": $("#time").val()}},
      function(r) {
        if (r.RespTime) {
          $("#parsed-time").text(r.RespTime);
        }
      })
  });
});

