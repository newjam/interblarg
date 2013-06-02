
var baseURL = document.URL
var stream = new EventSource(baseURL + '/stream')

stream.onerror = function(e) {
  throw "EventSource error"
}

stream.onmessage = function(e) { addMessage(e.data) }

addMessage = function(msg) {
  $('#inputDiv').after('<div>' + msg + '</div>')
}

Zepto(function($){
  $('#input').on('keypress', function(e){
    if(e.keyCode == '13') {
      $.post(baseURL, this.value, function(r){
        if(r === 'ok' ){
          $('#input').val('')
        }
      })
    }
  })


  // load the previous messages
  $.getJSON(baseURL + '/recent', function(data) {
    data.reverse()
    data.forEach(addMessage)
  })

})
