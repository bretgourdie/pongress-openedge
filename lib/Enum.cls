/**********************************************************************
 * Copyright (C) 2006-2010 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/  
/*------------------------------------------------------------------------
    File        : Enum
    Purpose     : Abstract base type for Enum classes
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 25 10:30:24 CEST 2010
    Notes       : http://blog.consultingwerk.de/consultingwerkblog/2010/08/a-pattern-for-enums
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS lib.Enum ABSTRACT: 

    /* Instance members */
    DEFINE PROTECTED PROPERTY Label AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.
    
    DEFINE PUBLIC PROPERTY Value AS INTEGER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Compares this Enum member to another member of this Enum by Value                                                                        
        Notes:   Returns FALSE when the passed in object reference is not of this
                 Enum type                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC LOGICAL Equals (INPUT poParam AS Progress.Lang.Object):
        
        IF NOT poParam:GetClass() = THIS-OBJECT:GetClass() THEN 
            RETURN FALSE . 
        
        RETURN CAST (poParam, lib.Enum):Value = THIS-OBJECT:Value .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CHARACTER representation (human readable) of the 
                 Enum member                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER ToString ():
        
        RETURN THIS-OBJECT:Label .

    END METHOD.

END CLASS.
