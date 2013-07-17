
var baseURL = document.URL
var stream = new EventSource(baseURL + '/stream')

var imgRegex = RegExp(/(http[s]?:\/\/.*\.(?:png|jpg|jpeg|gif))/)
var urlRegex = RegExp(/((ftp|http|https):\/\/[^ "]+)/)

stream.onerror = function(e) {
  // try again?
  stream = new EventSource(baseURL + '/stream')
  console.log('EventSource error')
}

stream.addEventListener("newMessage", function(e){
  addMessage($.parseJSON(e.data))
})

stream.addEventListener("updateUser", function(e){
  updateUser($.parseJSON(e.data))
})



wrapUrls = function(x){
  return x.replace(urlRegex, function(url){  
    console.log(url)
    if(imgRegex.test(url)){
      return $('<img/>', {src:url}).prop('outerHTML') //"<img src='" + url + "'/>"
    }else {
      return $('<a/>', {href:url, text:url}).prop('outerHTML')
    }
  })
}

changeName = function() {
  newName = $(this).val()

  $.post(baseURL + '/me', newName, function(r){ console.log(r) }) 

}

updateUser = function(data){
  console.log("user[" + data.id + "].name = '" + data.name + "'")
  var s = "div[data-user='"+ data.id +"'] > div.itemHeader > div.name"
  $(s+'.input > input').not(':focus').val(data.name)
  $(s+'.display').text(data.name) 
 
//  var x = $("div[data-user='"+ data.id +"'] > div.itemHeader > div.name").text(data.name)
//div.name input")/*.filter(':not(:focus)')*/.val(data.name)
}

addMessage = function(msg) {
  console.log(msg)

//  var obj =  0$.parseJSON(msg)

  var time = $('<div/>').addClass('time').text(moment.unix(msg.time).fromNow())
  var name = $('<div/>').addClass('name')

  var head = $('<div/>').addClass('itemHeader').append(name).append(time)
    
  var body = $('<div/>').html(wrapUrls(msg.payload))

  var x = $('<div/>').append(head).append(body).data('user', msg.user)

  $('#inputDiv').after(x)

  $.getJSON(baseURL + '/' + msg.user, function(userInfo){
    x.addClass('color' + userInfo.color)

    if(userInfo.id === me){
      name.addClass('input').append($('<input/>').val(userInfo.name).keyup(changeName))
    }else{
      name.addClass('display').text(userInfo.name)
    }
    
  })
}

;(function($){
  $.extend($.fn, {
    onEnter: function(f){
      $(this).on('keypress', function(e){
        if(e.keyCode == '13') {
          f.call(this)
        }
      })
      return this
    }
  })
})(Zepto)

Zepto(function($){
 
  $.getJSON(baseURL + '/me', function(id){
    me = id
  })
 
  $('#input').onEnter(function(){
    $.post(baseURL, this.value, function(r){
      if(r === 'ok' ){
        $('#input').val('')
      }
    })
  })
 

  x = $.getJSON(baseURL + '/recent')

  // load the previous messages
  $.getJSON(baseURL + '/recent', function(data) {
    data.reverse()
    data.forEach(addMessage)
  })
})
