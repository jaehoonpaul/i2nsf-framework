/*
 * This file is part of the nivo project.
 *
 * Copyright 2016-present, Raphaël Benitte.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
import React, { useState, useEffect} from 'react';
import { Component } from 'react'
import range from 'lodash/range'
import last from 'lodash/last'
import { generateDrinkStats } from '@nivo/generators'
import * as time from 'd3-time'
import { timeFormat } from 'd3-time-format'
// import { Line } from '../src'
import { Line } from '@nivo/line'
import Axios from 'axios';

const data = generateDrinkStats(18)

const commonProperties = {
    width: 900,
    height: 400,
    margin: { top: 20, right: 20, bottom: 60, left: 80 },
    data,
    animate: true,
    enableSlices: 'x',
}


const RealTimeChart6 = () =>{

    const date = new Date()
    date.setMinutes(0)
    date.setSeconds(0)
    date.setMilliseconds(0)

    const [olddata,setolddata] = useState(0)
    const [mdata,setmdata] = useState('')
    const [cpu,setcpu] = useState(0)
    const [memory,setmemory]=useState(0)
    const [disk,setdisk]=useState(0)
    const [outtraffic,setouttraffic]=useState(0)
    const [intraffic,setintraffic]=useState(0)
    const [oldin,setoldin] =useState(0)
    const [oldout,setoldout] = useState(0)
// 30 
    const [data_dict, setDataDict] = useState({
        dataA: range(100).map(i => ({
            x: time.timeMinute.offset(date, i * 30),
            y: 0 + Math.round(1),
        })),
        dataB: range(100).map(i => ({
            x: time.timeMinute.offset(date, i * 30),
            y: 0 + Math.round(1),
        })),
        dataC: range(100).map(i => ({
            x: time.timeMinute.offset(date, i * 30),
            y: 0 + Math.round(1),
        })),
    });

    const [count, setCount] = useState(0);

   

    let formatTime = timeFormat('%Y %b %d')
//타임인터벌 1초마다.. next함수실행
    // componentDidMount() {
    //     this.timer = setInterval(this.next, 100)
    // }
//

let dataA = data_dict['dataA']
let dataB = data_dict['dataB']


    const next =() => {
        // console.log(dataA)
        // if (this.state != undefined) {
        // if (this.state != undefined)
	console.log(typeof(cpu))
        dataA = dataA.slice(1)
        
        dataA.push({
            x: time.timeMinute.offset(last(dataA).x, 30),
            y: 0 + Math.round((intraffic-oldin)/3),
        })
        dataB = dataB.slice(1)
        dataB.push({
            x: time.timeMinute.offset(last(dataB).x, 30),
            y: 0 + Math.round((outtraffic-oldout)/3),
        })

        setDataDict({ dataA, dataB})
        // }
    }



    useEffect(() => {
        const countdown = setInterval(() => {
            setCount((count) + 1);
        }, 1000);

	Axios.get('http://172.24.4.12:3001/timedataold').then((response)=>{
	 
	//console.log(response);
	 setmdata(response.data[0]);
	 setolddata( response.data[1]);

	console.log("frist " + mdata);
	console.log("2nd " + olddata);
	
	// Set State 
	// setcpu(mdata["cpu-usage"]);
	// setdisk(mdata["disk-usage"]);
	// setmemory(mdata["memory-usage"]);
	 setintraffic(mdata["in-traffic-speed"]);
	 setouttraffic(mdata["out-traffic-speed"]);

	// console.log("cpu-usage : "+cpu)
	// console.log("memory-usage : "+memory)
	//console.log("disk-usag : "+disk)

	setoldin(olddata["in-traffic-speed"])
	setoldout(olddata["out-traffic-speed"])
	 console.log("in-traffic-speed : " + intraffic)
	console.log("old in-traffic-speed : " +oldin)
	console.log("old out-traffic-speed : " +oldout)
	 console.log("out-traffic-speed : " + outtraffic)
	})
        next();
        return () => clearInterval(countdown);
    }, [count]);



    return (
        <Line
            {...commonProperties}
            margin={{ top: 30, right: 160, bottom: 60, left: 50 }}
            data={[
                { id: 'Input Traffic', data: dataA, color: "hsl(0, 50%, 50%)"},
		 { id: 'Output Traffic', data: dataB, color: "hsl(100, 50%, 50%)"},

            ]}
            xScale={{ type: 'time', format: 'native' }}
            yScale={{ type: 'linear', max: 1000 }}
            axisTop={{
                format: '',
                tickValues: 'every 2 hours',
            }}
            axisBottom={{
                format: '',
                tickValues: 'every 4 hours',
                legend: '',
                legendPosition: 'middle',
                legendOffset: 46,
            }}
            colors={{ datum: 'color' }}
            axisRight={{}}
            enablePoints={false}
            enableGridX={true}
            curve="monotoneX"
            animate={false}
            motionStiffness={120}
            motionDamping={50}
            isInteractive={false}
            enableSlices={false}
            useMesh={true}
            theme={{
                axis: { ticks: { text: { fontSize: 14 } } },
                grid: { line: { stroke: '#ddd', strokeDasharray: '1 2' } },
            }}
            legends={[
                {
                    dataFrom: 'keys',
                    anchor: 'bottom-right',
                    direction: 'column',
                    justify: false,
                    translateX: 150,
                    translateY: 0,
                    itemsSpacing: 2,
                    itemWidth: 110,
                    itemHeight: 20,
                    itemDirection: 'left-to-right',
                    itemOpacity: 0.85,
                    symbolSize: 20,
                    effects: [
                        {
                            on: 'hover',
                            style: {
                                itemOpacity: 1
                            }
                        }
                    ]
                 }
            ]}
        />
    )
}

export default RealTimeChart6;
