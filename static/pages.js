
(function ($) {
  var methods = {};
  methods.go = function() {
    if (this.size() == 0) {
      throw new Exception("Page not found");
    } else if (this.size() > 1) {
      throw new Exception("Cannot go to multiple pages at once");
    }
    var target = $(this.get(0));
    var data = target.data('pages');
    if (data.cur) {
      data.cur.hide();
    }
    data.cur = target;
    target.show();
  };
  methods.setup = function() {
    var data = {cur: null};
    return this.each(function() {
      $(this).hide().data('pages', data);
    });
  };
  $.fn.pages = function(method) {
    if (!methods[method]) {
      throw new Exception("Invalid method: " + method);
    }
    return methods[method].call(this);
  };
})(jQuery);

