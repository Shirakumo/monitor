if(console.log === undefined)
    log = ()=>{};
else
    log = function(){
        let args = Array.prototype.slice.call(arguments);
        args.unshift("[Monitor]");
        return console.log.apply(console, args);
    };

let universalOffset = 2208988800;
let universalTime = ()=>{
    return (Date.now() / 1000)+universalOffset;
};

class Series{
    constructor(element, id, interval, unit){
        log("Init", element);

        if(id === undefined) id = element.dataset.series;
        if(interval === undefined) interval = element.dataset.interval;
        if(unit === undefined) unit = element.dataset.unit;

        this.apiRoot = document.querySelector("head link[rel=api-root]").getAttribute("href");
        if(!this.apiRoot){
            log("Failed to retrieve API root. WTF?");
        }

        if(typeof interval === "string")
            interval = parseFloat(interval);
        this.id = id;
        this.element = element;
        this.interval = interval;
        this.unit = unit;
        this.last_check = universalTime() - (60*60*24);
        this.update().then(()=>{
            setInterval(()=>this.update(), Math.max(1000,Math.round(this.interval*1000)));
        });
        this.initChart();
    }

    apiCall(endpoint, args, methodArgs){
        methodArgs = methodArgs || {};
        methodArgs.format = methodArgs.format || "json";
        return new Promise((ok, fail)=>{
            let request = new XMLHttpRequest();
            let formData;

            if(!(endpoint.startsWith("http://") || endpoint.startsWith("https://"))){
                endpoint = this.apiRoot+endpoint;
            }

            if(args instanceof HTMLElement){
                formData = new FormData(args);
                formData.delete("browser");
            }else if(args instanceof FormData){
                formData = args;
            }else{
                formData = new FormData();
                for(let field in args){
                    formData.append(field, args[field]);
                }
            }

            if(methodArgs.format == "json")
                formData.append("data-format", "json");
            request.onload = ()=>{
                let data = request.responseText;
                let status = request.status;
                if(request.getResponseHeader("Content-Type").includes("application/json")){
                    data = JSON.parse(data);
                    status = data.status || status;
                }
                if(status === 200){
                    ok(data);
                }else{
                    log("Request failed", data);
                    fail(data);
                }
            };
            request.open("POST", endpoint);
            request.send(formData);
        });
    }

    initChart(){
        let formatter = (s, v) => v.toFixed(3);
        if(this.unit === "%") formatter = (s, v) => v.toFixed(1) + "%";
        if(this.unit === "kB") formatter = (s, v) => v.toFixed(1) + "kB";
        if(this.unit === "s") formatter = (s, v) => v.toFixed(3) + "s";
        let opts = {
            width: 400,
            height: 200,
            cursor: {
				drag: {
					setScale: false,
				}
			},
			select: {
				show: false,
			},
            series: [
                {},
                {
                    stroke: "red",
                    width: 1,
                    fill: "rgba(255, 0, 0, 0.3)",
                    values: formatter
                }
            ],
            scales: {},
            axes: [
                {},
                {scale: this.unit}
            ]
        };
        if(this.unit === "%")
            opts.scales["%"] = {auto: false, range: [0,100]};
        else
            opts.scales[this.unit] = {auto: true};
        this.data = [[],[]];
        this.uplot = new uPlot(opts, this.data, this.element);
    }

    update(){
        let since = this.last_check+"";
        this.last_check = universalTime();
        return this.apiCall("series/data", {id: this.id, since: since}, {format: "json"})
            .then((req)=>this.show(req.data));
    }

    show(data){
        let placeholder = this.element.querySelector(".placeholder");
        if(placeholder) placeholder.remove();
        if(data == null || data.data[0].length == 0)
            return;
        
        log(data.data[0].length,"new values.");
        if(this.data[0].length <= 0 || data.data[0][0] < this.data[0][this.data[0].length-1]){
            log("Got time before our last value, replacing.");
            this.data = [[],[]];
        }
        this.data[0] = this.data[0].concat(data.data[0]);
        this.data[1] = this.data[1].concat(data.data[1]);
        this.uplot.setData(this.data);
    }
}

let initMonitors = ()=>{
    log("Init");
    for(let el of document.querySelectorAll(".series.widget")){
        new Series(el);
    }
};

document.addEventListener("DOMContentLoaded", initMonitors);
