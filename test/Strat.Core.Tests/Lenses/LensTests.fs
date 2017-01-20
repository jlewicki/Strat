namespace Strat.Lenses.Tests

open Strat.Lenses
open Strat.Lenses.Lens
open Xunit


[<AutoOpen>]
module Fixture = 
   type State = {
      ID: int
      Name: string
   }


   type Address = {
      Street: string
      City: string
      State: State
      AptNumber: option<string>
   }
   
   type Person = {
      FirstName: string
      LastName: string
      Age: int
      Address: Address
      Address2: option<Address>
   }

   let ma = { ID = 1; Name = "Massachusetts"}
   let addr1 = { Street = "Regent St"; City = "Cambridge"; State = ma; AptNumber = None}
   let addr2 = { Street = "Rogers Ave"; City = "Somerville"; State = ma; AptNumber = Some("2A") }   
   let person1 = { FirstName="Carolyn"; LastName="Zeiner"; Age=35; Address = addr1; Address2 = None }
   let person2 = { FirstName="Keith"; LastName="Clemens"; Age=35; Address = addr1; Address2 = Some(addr2) }
   let person3 = { person1 with Address = addr2 }

   let firstNameLens = fromGetSet (fun (p:Person) -> p.FirstName) (fun v (p:Person) -> { p with FirstName = v } )
   let addressLens = fromGetSet (fun (p:Person) -> p.Address) (fun v (p:Person) -> { p with Address = v } )
   let address2Lens = fromGetSetPartial (fun (p:Person) -> p.Address2) (fun v (p:Person) -> { p with Address2 = Some(v) } )
   let stateLens = fromGetSet (fun (a:Address) -> a.State) (fun v (a:Address) -> { a with State = v } )
   let cityLens = fromGetSet (fun (a:Address) -> a.City) (fun v (a:Address) -> { a with City = v } )
   let aptNumLens = fromGetSetPartial (fun (a:Address) -> a.AptNumber) (fun v (a:Address) -> { a with AptNumber = Some(v) } )
   let stateNameLens = fromGetSet (fun (a:State) -> a.Name) (fun v (a:State) -> { a with Name = v } )


module FromGetSet = 
   
   [<Fact>]
   let Should_Create_Lens_That_Can_Get() =
      let cityLens = fromGetSet (fun (a:Address) -> a.City) (fun v (a:Address) -> { a with City = v } )
      
      let city = addr1 |-> cityLens
      Assert.Equal<string>("Cambridge",  city)
      
   
   [<Fact>]
   let Should_Create_Lens_That_Can_Set() =
      let cityLens = fromGetSet (fun (a:Address) -> a.City) (fun v (a:Address) -> { a with City = v } )
            
      let addr = addr1 |> set cityLens "Boston"
      let city = addr |-> cityLens
      Assert.Equal<string>("Boston",  city)


module Set = 
   
   [<Fact>]
   let Should_Set_Value() = 
      let person =  person1 |> set firstNameLens "Maggie"
      Assert.Equal<string>("Maggie",  person.FirstName)


   [<Fact>]
   let Should_Set_Value_With_Operator() = 
      let person = (firstNameLens <-| "Maggie") person1
      Assert.Equal<string>("Maggie",  person.FirstName)


module Update = 
   
   [<Fact>]
   let Should_Set_Value() = 
      let person =  person1 |> update firstNameLens (fun name -> name.ToUpper())
      Assert.Equal<string>("CAROLYN",  person.FirstName)



module Compose = 
   
   [<Fact>]
   let Should_Compose_Total_Lenses() = 
      let addressCityLens = addressLens --> cityLens
      let city = person1 |-> addressCityLens

      Assert.Equal<string>("Cambridge",  city)

   [<Fact>]
   let Should_Compose_Partial_Total_Lenses() = 
      let address2CityLens = address2Lens ?-> cityLens
      
      // Get when partial lens returns None
      let city = person1 |> getPartial address2CityLens
      Assert.Equal(None,  city)

      // Get when partial lens returns Some
      let city = person2 |> getPartial address2CityLens
      Assert.Equal(Some("Somerville"),  city)

      // Set when partial lens returns None
      let person = person1 |> setPartial address2CityLens "Boston"
      let city = person |> getPartial address2CityLens
      Assert.Equal(None, city)

      // Set when partial lens returns Some
      let person = person2 |> setPartial address2CityLens "Boston"
      let city = person |> getPartial address2CityLens
      Assert.Equal(Some("Boston"), city)


   [<Fact>]
   let Should_Compose_Total_Partial_Lenses() = 
      let addressAptNumberLens = addressLens -?> aptNumLens
      
      // Get when partial lens returns None
      let aptNum = person1 |> getPartial addressAptNumberLens
      Assert.Equal(None, aptNum)

      // Get when partial lens returns Some
      let aptNum = person3 |> getPartial addressAptNumberLens
      Assert.Equal(Some("2A"), aptNum)

      // Set
      let person = person1 |> setPartial addressAptNumberLens "1C"
      let aptNum = person |> getPartial addressAptNumberLens
      Assert.Equal(Some("1C"), aptNum)


   [<Fact>]
   let Should_Compose_Partial_Lenses() = 
      let address2AptNumberLens = address2Lens ??> aptNumLens
      
      // Get when partial lens 1 returns None
      let aptNum = person1 |> getPartial address2AptNumberLens
      Assert.Equal(None, aptNum)

      // Get when partial lenses return Some
      let aptNum = person2 |> getPartial address2AptNumberLens
      Assert.Equal(Some("2A"), aptNum)

      // Set when partial lens 1 returns None
      let person = person1 |> setPartial address2AptNumberLens "1C"
      let aptNum = person |> getPartial address2AptNumberLens
      Assert.Equal(None, aptNum)

      // Set when partial lens 1 returns Some
      let person = person2 |> setPartial address2AptNumberLens "1C"
      let aptNum = person |> getPartial address2AptNumberLens
      Assert.Equal(Some("1C"), aptNum)


   [<Fact>]
   let Should_Be_Left_Associative() = 
      let address1StateNameLens = addressLens --> stateLens --> stateNameLens

      // Get when partial lens 1 returns None
      let stateName = person1 |-> address1StateNameLens
      Assert.Equal<string>("Massachusetts", stateName)