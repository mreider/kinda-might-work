

var authenticationSuccess = function(state){

      return function(){
            window.location = '/tr-clb?code='+Trello.token()+'&state='+state;
      }
};


var authenticationFailure = function(y){ 
      console.log('Failed authentication:' + y); 
};


function authenticateTrello(state){

  Trello.authorize({ type: 'popup'
                   , name: 'Kinda might work'
                   , persist: false
                   , scope: { read: 'true'
                            , write: 'true' 
                            }
                   , expiration: 'never'
                   , success: authenticationSuccess(state)
                   , error: authenticationFailure
                   }
                  );
}