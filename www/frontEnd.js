//startup
var canvas=document.getElementById("canvas");
var c=canvas.getContext("2d");

//engine support
var nodes=Array();
var selected=null;
var mouse=null;
function formatClick(e){
	var b=canvas.getBoundingClientRect();
	e.x=e.clientX-b.left;
	e.y=e.clientY-b.top;
}
function mousedown(e){
	formatClick(e);
	for(var a=0;a<nodes.length;a++){
		if(distance(nodes[a],e.x,e.y)<nodes[a].radius){
			selected=nodes[a];
			//lastmouse=[e.x,e.y];
			return;
		}
	}
}
function mousemove(e){
	formatClick(e);
	if(selected!=null){
		selected.x+=e.x-mouse[0];
		selected.y+=e.y-mouse[1];
	}
	mouse=[e.x,e.y];
}
function clickCanvas(){
	var x=mouse[0];
	var y=mouse[1];
	for(var a=0;a<nodes.length;a++){
		var node=nodes[a];
		if(distance(node,x,y)<=node.radius){
			Shiny.onInputChange("gene",node.name);
			Shiny.onInputChange("gene1","none");
			return;
		}
		for(var b=0;b<a;b++){
			if(edgeClick(node,nodes[b])){
				return;
			}
		}
		for(var b=a+1;b<nodes.length;b++){
			if(edgeClick(node,nodes[b])){
				return;
			}
		}
	}
}
function edgeClick(node,node1){
	var x=mouse[0];
	var y=mouse[1];
	if((x>node.x)!=(x>node1.x)){
		var m=(node.y-node1.y)/(node.x-node1.x);
		var expectedY;
		if(node.x<node1.x){
			expectedY=(m*(x-node.x))+node.y;
		}else{
			expectedY=(m*(x-node1.x))+node1.y;
		}
		if(Math.abs(y-expectedY)<4){
			Shiny.onInputChange("gene",node.name);
			Shiny.onInputChange("gene1",node1.name);
			return true;
		}
	}
	return false;
}
function distance(){
	function dist(x,y,x1,y1){
		return Math.sqrt(Math.pow(x-x1,2)+Math.pow(y-y1,2));
	}
	if(arguments.length==4){
		return dist(arguments[0],arguments[1],arguments[2],arguments[3]);
	}
	if(arguments.length==3){
		return dist(arguments[0].x,arguments[0].y,arguments[1],arguments[2]);
	}
	if(arguments.length==2){
		return dist(arguments[0].x,arguments[0].y,arguments[1].x,arguments[1].y);
	}
}

//class definitions
function Node(name,x,y){
	//Node.radius=10;
	this.radius=(c.measureText(name).width/2)+5;
	this.x=x;
	this.y=y;
	this.name=name;
	this.color=getColor(name);
	this.connections=new Array();
	if(arguments.length>3){
		for(var a=3;a<arguments.length;a++){
			this.connections.push(arguments[a]);
		}
	}
}

//nodes support
function getColor(name){
	var colors=["red","blue","green","orange","purple","yellow"];
	return colors[Math.floor(Math.random()*colors.length)];
}
function drawNodes(){
	for(var a=0;a<nodes.length;a++){
		var node=nodes[a];
		c.fillStyle=node.color;
		c.beginPath();
		c.arc(node.x,node.y,node.radius,0,2*Math.PI,true);
		c.closePath();
		c.fill();
		c.fillStyle="black";
		c.fillText(node.name,node.x-(node.radius-5),node.y+5);
	}
}
function drawEdges(){
	c.strokeStyle="gray";
	c.lineWidth=3;
	for(var a=0;a<nodes.length;a++){
		var node=nodes[a];
		for(var b=0;b<node.connections.length;b++){
			var node1=nodes[node.connections[b]];
			c.beginPath();
			c.moveTo(node.x,node.y);
			c.lineTo(node1.x,node1.y);
			c.stroke();
			c.closePath();
		}
	}
}
function setEdges(){
	for(var a=0;a<nodes.length;a++){
		for(var b=0;b<nodes[a].connections.length;b++){
			for(var c=0;c<nodes.length;c++){
				if(nodes[c].name==nodes[a].connections[b]){
					nodes[a].connections[b]=c;
				}
			}
		}
	}
}

//initialize
function update(){
	c.clearRect(0,0,canvas.width,canvas.height);
	drawEdges();
	drawNodes();
	c.strokeRect(0,0,canvas.width,canvas.height);
	setTimeout(update,100);
}
c.font="10pt bold";
nodes.push(new Node("age",10,10,"wt.loss","meal.cal","sex"));
nodes.push(new Node("wt.loss",30,10,"meal.cal","sex"));
nodes.push(new Node("meal.cal",50,10,"sex"));
nodes.push(new Node("sex",70,10));
setEdges();
update();