/*    -*- Java -*-
 *
 *  Copyright 2007 Tail-F Systems AB. All rights reserved.
 *
 *  This software is the confidential and proprietary
 *  information of Tail-F Systems AB.
 *
 *  $Id$
 *
 */

import java.net.Socket;

import org.apache.log4j.Logger;

import com.tailf.conf.Conf;
import com.tailf.dp.Dp;

public class Actions implements Runnable {

    private static Logger LOGGER = Logger.getLogger(Actions.class);

    private Socket   ctrl_socket;
    private Dp       dp;

    public Actions() {

        try {

            /* create the callbacks */
            slowTrans action_cb = new slowTrans();

            /* create new control socket */
            ctrl_socket= new Socket("127.0.0.1",Conf.PORT);

            /* init and connect the control socket */
            dp = new Dp("ServerDemonDP3Test",ctrl_socket);
            // Dp dp = new Dp("server_daemon",ctrl_socket);

            /* register the callbacks */
            dp.registerAnnotatedCallbacks(action_cb);

            dp.registerDone();

            /* read input from control socket */
            //while (true) dp.read();

        } catch (Exception e) {

            LOGGER.error("(closing) " +e.getMessage());
        }
    }


    public void run(){
        try{
            while(true) dp.read();


        }catch(Exception e){
            LOGGER.error(e);
            System.exit(0);
        }

    }

    static public void main(String[] args) {
        Actions a = new Actions();

        // write message to signal provider running - test purposes
        System.out.println("---PROVIDER-RUNNING---");

        new Thread(a).start();
    }

}
