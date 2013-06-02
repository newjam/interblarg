
var baseURL = document.URL
var stream = new EventSource(baseURL + '/stream')

var imgRegex = RegExp('(http[s]?:\/\/.*\.(?:png|jpg|jpeg|gif))')

stream.onerror = function(e) {
  // try again?
  stream = new EventSource(baseURL + '/stream')
  console.log('EventSource error')
}

stream.onmessage = function(e) { addMessage(e.data) }

addMessage = function(msg) {
  inner = imgRegex.test(msg) ? "<img src='" + msg + "'/>" : msg

  $('#inputDiv').after('<div>' + inner + '</div>')
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
