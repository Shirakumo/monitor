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
let hashColor = (str)=>{
    let hash = str.split('').reduce((a,b)=>{a=((a<<5)-a)+b.charCodeAt(0);return a&a},0);
    let colors = ["0,63,92","47,75,124","102,81,145","160,81,149","212,80,135","249,93,106","255,124,67","255,166,0"];
    return colors[Math.abs(hash % colors.length)];
};
let defaultTimeRange = 60*60*12;
let formatPercentage = (s, v) => {
    return v.toFixed(1) + "%";
};
let formatSeconds = (s, v) => {
    if(v === null) return '';
    if(v < 0) return Math.round(v*1000) + "ms";
    if(v < 60) return v.toFixed(1) + "s";
    v /= 60;
    if(v < 60) return v.toFixed(1) + "m";
    v /= 60;
    return v.toFixed(1) + "h";
};
let formatBytes = (s, v) => {
    if(v === null) return '';
    if(v < 1024) return Math.round(v) + "B";
    v /= 1024;
    if(v < 1024) return Math.round(v) + "kB";
    v /= 1024;
    if(v < 1024) return Math.round(v) + "MB";
    v /= 1024;
    return Math.round(v) + "GB";
};

class Series{
    constructor(element, options){
        log("Init", element);

        options = options | {};
        let id = options["series"];
        let interval = options["interval"];
        let unit = options["unit"];
        let color = options["color"];
        if(id === undefined) id = element.dataset.series;
        if(interval === undefined) interval = element.dataset.interval;
        if(unit === undefined) unit = element.dataset.unit;
        if(color === undefined) color = element.querySelector("header i") ? hashColor(element.querySelector("header i").getAttribute("class")) : "255,255,255";

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
        this.color = color;
        this.last_check = universalTime() - defaultTimeRange;
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
        let splits = [];
        if(this.unit === "%"){
            formatter = formatPercentage;
            splits = [0,25,50,75,100];
        }
        if(this.unit === "s"){
            formatter = formatSeconds;
            splits = [0.0,0.1,0.25,0.5,0.75,1.0];
        }
        if(this.unit === "B"){
            formatter = formatBytes;
            splits = [1,32,1024,1024*32,1024*1024,1024*1024*32,1024*1024*1024];
        }
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
                    width: 1,
                    stroke: "rgb("+this.color+")",
                    fill: "rgba("+this.color+",0.2)",
                    scale: this.unit,
                    values: formatter
                }
            ],
            axes: [
                {stroke: "white"},
                {
                    scale: this.unit,
                    stroke: "white",
                    splits: splits,
                    filter: (u, v) => v,
                    values: (u, s) => s.map((v, i) => formatter(u, v)),
                }
            ],
            scales: {
                "%": {
                    auto: false,
                    range: [0,100]
                },
                "s": {
                    auto: true
                },
                "B": {
                    auto: false,
                    range: [1, 1024*1024*1024],
                    distr: 3,
                    log: 2
                }
            },
        };
        this.data = [[],[]];
        this.uplot = new uPlot(opts, this.data, this.element);
        this.uplot.opts = opts;
        
        new ResizeObserver(()=>{
            let size = {
                width: this.element.clientWidth,
                height: Math.min(this.element.clientWidth / 2, 500)
            };
            log("Resizing to", size);
			this.uplot.setSize(size);
		}).observe(this.element);
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
        
        if(this.data[0].length <= 0){
            let last = this.data[0][this.data[0].length-1];
            let i = 0;
            while( i < data.data[0].length && data.data[0][i] < last){
                i++;
            }
            data.data[0] = data.data[0].slice(i);
            data.data[1] = data.data[1].slice(i);
        }
        if(this.uplot.opts.scales[this.unit].distr === 3){
            for(let i=0; i<data.data[1].length; ++i){
                if(data.data[1][i] == 0.0) data.data[1][i] = 1.0;
            }
        }
        log(data.data[0].length,"new values.");
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
