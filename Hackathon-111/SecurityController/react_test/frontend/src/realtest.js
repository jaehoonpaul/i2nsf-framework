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

const data = generateDrinkStats(18)

const commonProperties = {
    width: 900,
    height: 400,
    margin: { top: 20, right: 20, bottom: 60, left: 80 },
    data,
    animate: true,
    enableSlices: 'x',
}


const RealTimeChart = () =>{

    const date = new Date()
    date.setMinutes(0)
    date.setSeconds(0)
    date.setMilliseconds(0)

    const [data_dict, setDataDict] = useState({
        dataA: range(100).map(i => ({
            x: time.timeMinute.offset(date, i * 30),
            y: 10 + Math.round(1 * 20),
        })),
        dataB: range(100).map(i => ({
            x: time.timeMinute.offset(date, i * 30),
            y: 50 + Math.round(Math.random() * 20),
        })),
        dataC: range(100).map(i => ({
            x: time.timeMinute.offset(date, i * 30),
            y: 60 + Math.round(Math.random() * 20),
        })),
    });

    //시간
    const [count, setCount] = useState(0);

   

    let formatTime = timeFormat('%Y %b %d')
//타임인터벌 1초마다.. next함수실행
    // componentDidMount() {
    //     this.timer = setInterval(this.next, 100)
    // }
//

let dataA = data_dict['dataA']
let dataB = data_dict['dataB']
let dataC = data_dict['dataC']


    const next =() => {
        // console.log(dataA)
        // if (this.state != undefined) {
        // if (this.state != undefined)
        dataA = dataA.slice(1)
        
        dataA.push({
            x: time.timeMinute.offset(last(dataA).x, 30),
            y: 10 + Math.round(Math.random() * 20),
        })
        dataB = dataB.slice(1)
        dataB.push({
            x: time.timeMinute.offset(last(dataB).x, 30),
            y: 30 + Math.round(Math.random() * 20),
        })
        dataC = dataC.slice(1)
        dataC.push({
            x: time.timeMinute.offset(last(dataC).x, 30),
            y: 60 + Math.round(Math.random() * 20),
        })

        setDataDict({ dataA, dataB, dataC })
        // }
    }



    useEffect(() => {
        const countdown = setInterval(() => {
            setCount((count) + 1);
        }, 100);
        
        console.log(count)
        next();

        return () => clearInterval(countdown);
    }, [count]);

    return (
        <Line
            {...commonProperties}
            margin={{ top: 30, right: 50, bottom: 60, left: 50 }}
            data={[
                { id: 'A', data: dataA, color: "hsl(150, 50%, 50%)"},
                { id: 'B', data: dataB, color: "hsl(200, 50%, 50%)"},
                { id: 'C', data: dataC, color: "hsl(250, 50%, 50%)"},
            ]}
            xScale={{ type: 'time', format: 'native' }}
            yScale={{ type: 'linear', max: 100 }}
            axisTop={{
                format: '%H:%M',
                tickValues: 'every 4 hours',
            }}
            axisBottom={{
                format: '%H:%M',
                tickValues: 'every 4 hours',
                legend: `${formatTime(dataA[0].x)} ——— ${formatTime(last(dataA).x)}`,
                legend: `1 ——— 2`,
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
        />
    )
}

export default RealTimeChart;
