

var authenticationSuccess = function(x){ 
      //window.location = 'tr-clb?code='+x+'&state=';
};


var authenticationFailure = function(y){ 
      console.log('Failed authentication:' + y); 
};


function authenticateTrello(){

  Trello.authorize({ type: 'popup'
                   , name: 'Kinda might work'
                   , persist: false
                   , scope: { read: 'true'
                            , write: 'true' 
                            }
                   , expiration: 'never'
                   , success: authenticationSuccess
                   , error: authenticationFailure
                   }
                  );
}