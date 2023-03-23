# -*- coding: utf-8 -*-
"""
Created on Mon May 30 17:29:28 2022

@author: Patrick
"""

from tkinter import Button, Tk, mainloop, Label, Entry


def Translate():
    print("Translating")


class menu():
    app = Tk()
    app.geometry("800x600")
    app.title("I2NSF Security Policy Translator")
    
    heading = Label(app, text="High-Level Policy Input")
    policyName = Label(app, text="Policy Name")
    ruleName = Label(app,text="Rule Name")
    condition = Label(app, text="Condition")
    source = Label(app,text="Source")

    heading.grid(row=1, column=1)
    policyName.grid(row=2,column=0)
    ruleName.grid(row=3,column=0)
    condition.grid(row=4,column=0)
    source.grid(row=5,column=0)
    
        
    policyName_field = Entry(app)
    ruleName_field = Entry(app)
    source_field = Entry(app)
    
    policyName_field.grid(row=2,column=1)
    ruleName_field.grid(row=3,column=1)
    source_field.grid(row=5,column=1)
    
    #load the buttons for UI
    bLoad = Button(app, text = "Translate", command = Translate())
    bLoad.grid(row=6, column=1)
    
if __name__ == '__main__':
    mainloop()