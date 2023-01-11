What is EML?
Entity Manipulation Language (EML) is a part of the ABAP language. It means that there are new syntaxes available in ABAP to

control the business object’s behavior in the context of ABAP RESTful programming model, using ABAP code user can manipulate Business Object created using ABAP RAP model
provides a type-save read and modifying access to data in transactional development scenarios.
Where%20EML%20comes%20into%20picture

Where EML comes into the picture

How EML syntaxes look like?
EML contains syntaxes to READ, MODIFY (Create, Update, Delete, Execute Actions) and COMMIT. Let’s understand each syntax step by step

READ ENTITIES
Read%20Entities%20Syntax

MODIFY ENTITIES – CREATE


MODIFY ENTITIES – UPDATE


MODIFY ENTITIES – DELETE


MODIFY ENTITIES – ACTION


 

Commit Entities syntax is clubbed with Modify syntaxes to logically complete the flow. LUW concept is there in all the MODIFY syntaxes.

MODIFY syntax manipulates Transactional Buffer and READ also get data from Transactional Buffer. In the case of Managed Behavior, managed runtime automatically populates data in Transactional buffer and in case of Unmanaged Behavior, a user needs to implement Transactional READ method of Behavior Implementation

Where is EML used?
Usually, Business objects that are implemented with the ABAP RESTful architecture based on the behavior definition and implementation of the interaction phase and save sequence in behavior pools can be consumed by means of OData protocol (Fiori UIs, or Web APIs)

Now to access them through ABAP code EML syntax is used.

The standard API is used whenever the “target” business object is statically specified. It provides code completion and static code checks. This typed API provides statements for read-only access to data (READ ENTITIES), as well as for modifying data access (MODIFY ENTITIES) and for triggering the save sequence (COMMIT ENTITIES), these all we saw above

The generic API is typically used for generic integration of business objects into other frameworks, such as the Cloud Data Migration Cockpit or the Process Test Framework.

What is the Use Case?
One of the uses cases of EML is the writing of test modules as ABAP Unit tests. As ABAP application developer, it gives you the ability to test the transactional behavior of business objects for each relevant operation that is defined in the behavior definition.

Another use-case is cross BO integration. Executing CRUDA operation of RAP based BO in another RAP based BO’ Implementation in exits like determination, validation, actions, etc.

 

From which release it is available?
EML – Entity Manipulation Language is available after 1902 Cloud release and 1909 on-premise release

 

Few points to note
Multiple modify elements can be used in one MODIFY statement. The sequence of the operations is irrelevant.
EML syntaxes for LUW concepts
Authorizations will be automatically handled by these syntaxes, if a user needs to escape authorizations then just add syntax – IN LOCAL MODE
READ by associations also works. From a parent entity, child entity can be read and vice versa.
