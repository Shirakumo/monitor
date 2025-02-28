class Series{
    constructor(element, id, interval){
        if(console.log === undefined)
            this.log = ()=>{};
        else
            this.log = function(){
                var args = Array.prototype.slice.call(arguments);
                args.unshift("[Monitor]");
                return console.log.apply(console, args);
            };
        
        this.log("Init", element);

        if(id === undefined) id = element.dataset.series;
        if(interval === undefined) interval = element.dataset.interval;

        this.apiRoot = document.querySelector("head link[rel=api-root]").getAttribute("href");
        if(!self.apiRoot){
            self.log("Failed to retrieve API root. WTF?");
        }

        this.id = id;
        this.element = element;
        this.interval = interval;
        this.last_check = 0 - interval*100;
        this.update();
    }

    apiCall(endpoint, args, methodArgs){
        var self = this;
        methodArgs = methodArgs || {};
        methodArgs.format = methodArgs.format || "json";
        return new Promise((ok, fail)=>{
            var request = new XMLHttpRequest();
            var formData;

            if(!(endpoint.startsWith("http://") || endpoint.startsWith("https://"))){
                endpoint = self.apiRoot+endpoint;
            }

            if(args instanceof HTMLElement){
                formData = new FormData(args);
                formData.delete("browser");
            }else if(args instanceof FormData){
                formData = args;
            }else{
                formData = new FormData();
                for(var field in args){
                    formData.append(field, args[field]);
                }
            }

            if(methodArgs.format == "json")
                formData.append("data-format", "json");
            request.onload = ()=>{
                var data = request.responseText;
                var status = request.status;
                if(request.getResponseHeader("Content-Type").includes("application/json")){
                    data = JSON.parse(data);
                    status = data.status || status;
                }
                if(status === 200){
                    self.log("Request succeeded", data);
                    ok(data);
                }else{
                    self.log("Request failed", data);
                    fail(data);
                }
            };
            self.log("Sending request to",endpoint);
            request.open("POST", endpoint);
            request.send(formData);
        });
    }

    update(){
        this.apiCall("monitor/series/data", {id: self.id, since: this.last_check+""}, {format: "json"})
            .then((data)=>{
                this.show(data);
            });
    }

    show(data){
        
    }
}

for(let el of document.querySelectorAll(".series")){
    new Series(el);
}
